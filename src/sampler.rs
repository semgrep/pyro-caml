use std::{
    cell::RefCell,
    ops::Deref,
    path::{Path, PathBuf},
    sync::{
        Arc, Mutex, OnceLock,
        atomic::{AtomicBool, Ordering},
    },
    thread::{self, JoinHandle},
};

use ocaml::Runtime;
use pyroscope::{
    PyroscopeError,
    backend::{BackendConfig, StackBuffer, StackFrame, StackTrace, ThreadTagsSet},
    error::Result,
};

use crate::ocaml_intf;

const LOG_TAG: &str = "Pyro_caml::sampler";

// Runtime doesn't implement send, it is really unstable across threads if
// it's been initialized in one but not the other due to how ocaml domain
// locks are implemented. So we cheat here and use a thread local runtime
thread_local! {
    static OCAML_GC: RefCell<Runtime> =  RefCell::new(ocaml::init()) ;
    static OCAML_CURSOR: OnceLock<ocaml_intf::Cursor> = const { OnceLock::new() };
}

#[derive(Debug, Clone)]
pub struct SamplerConfig {
    pub sample_rate: u32,
    pub max_delta: u32,
    pub pid: u32,
    pub event_directory: PathBuf,
}

impl SamplerConfig {
    pub fn get_event_file_path(&self) -> PathBuf {
        // file is always pid.events
        let mut path = self.event_directory.clone();
        path.push(format!("{}.events", self.pid));
        path
    }
    fn acquire_cursor(&self) -> ocaml_intf::Cursor {
        OCAML_CURSOR.with(|cell| {
            cell.get_or_init(|| {
                OCAML_GC.with_borrow(|gc| {
                    let event_file = self.get_event_file_path();
                    // wait for the file to appear
                    log::debug!(target:LOG_TAG, "waiting for event file to appear: {:?}", event_file);
                    while !Path::new(&event_file).exists() {
                        std::thread::sleep(std::time::Duration::from_millis(100));
                    }
                    log::debug!(target:LOG_TAG, "event file found: {:?}", event_file);
                    ocaml_intf::create_cursor(gc, &self.event_directory , self.pid)
                })
            })
            .clone()
        })
    }

    fn sample_interval(&self) -> f64 {
        1.0 / self.sample_rate as f64
    }

    fn max_delta(&self) -> f64 {
        self.max_delta as f64 / 1_000_000.0
    }
}

struct AliveGuard(Arc<AtomicBool>);
impl Drop for AliveGuard {
    fn drop(&mut self) {
        self.0.store(false, Ordering::Relaxed);
    }
}

pub struct Sampler {
    running: Arc<AtomicBool>,
    thread: Option<JoinHandle<()>>,
}

impl Sampler {
    /// Sampler takes in buffers, each corresponding to a single backend.
    /// It drains the OCaml event buffer and fans each sample out into one
    /// buffer per backend. This makes it possible to support multiple
    /// different backends simultaneously (cpu, alloc_space, inuse_space, etc.)
    pub fn start(
        config: SamplerConfig,
        backend_config: Arc<Mutex<BackendConfig>>,
        tagset: Arc<Mutex<ThreadTagsSet>>,
        alive: Arc<AtomicBool>,
        buffers: Vec<Arc<Mutex<StackBuffer>>>,
    ) -> Self {
        let running = Arc::new(AtomicBool::new(true));
        let thread_running = running.clone();
        alive.store(true, Ordering::Relaxed);

        let empty_frame = StackFrame {
            module: None,
            name: Some("unknown".to_string()),
            filename: None,
            relative_path: None,
            absolute_path: None,
            line: None,
        };

        let empty_stack_trace = StackTrace::new(
            &backend_config.lock().unwrap(),
            Some(config.pid),
            None,
            None,
            vec![empty_frame],
        );
        let thread = thread::spawn(move || {
            let _alive_guard = AliveGuard(alive);
            log::debug!(target:LOG_TAG, "starting sampler thread");
            while thread_running.load(Ordering::Relaxed) {
                log::trace!(target:LOG_TAG, "acquiring backend config and cursor");
                let cursor = config.acquire_cursor();
                log::trace!(target:LOG_TAG, "sampling...");
                let mut stack_frames: Vec<StackTrace> = {
                    let backend_config = backend_config
                        .lock()
                        .expect("Could not take backend config lock"); // we only have one sampler thread and one reporting thread so this should never fail unless something is seriously wrong

                    OCAML_GC
                    .with_borrow(|gc| {
                        ocaml_intf::read_poll(
                            gc,
                            cursor,
                            config.sample_interval(),
                            config.max_delta(),
                        )
                    })
                    .unwrap_or_else(|e| {
                        log::error!(target:LOG_TAG, "Error reading from OCaml runtime: {:?}", e);
                        vec![]
                    })
                    .into_iter()
                    .map(|st| st.into_stack_trace(backend_config.deref(), config.pid))
                    .collect()
                };

                log::trace!(target:LOG_TAG, "done sampling");

                if stack_frames.is_empty() {
                    // push an empty stack_trace to indicate idle
                    log::trace!(target:LOG_TAG, "no stack frames found, pushing empty stack trace");
                    stack_frames.push(empty_stack_trace.clone());
                }

                log::trace!(target:LOG_TAG, "recording stack frames");
                for st in stack_frames.into_iter() {
                    let stack_trace =
                        st.add_tag_rules(&tagset.lock().expect("Failed to lock ruleset"));
                    for buffer in &buffers {
                        buffer
                            .lock()
                            .expect("Failed to lock buffer") // we only have one sampler thread and one sending reports so we should never fail to obtain the lock unless something is seriously wrong
                            .record(stack_trace.clone())
                            .expect("Failed to record stack traces to buffer"); // So this seems to just be a result in the pyroscope sdk for no reason
                    }
                }
                log::trace!(target:LOG_TAG, "recorded stack trace");
                log::trace!(target:LOG_TAG, "sleeping for {} ms", 1000 / config.sample_rate);

                thread::sleep(std::time::Duration::from_millis(
                    1000 / config.sample_rate as u64,
                ));
            }
            log::debug!(target:LOG_TAG, "sampler thread exiting")
        });

        Sampler {
            running,
            thread: Some(thread),
        }
    }

    pub fn stop(self) -> Result<()> {
        log::trace!(target:LOG_TAG, "Shutting down sampler thread");
        self.running.store(false, Ordering::Relaxed);
        self.thread
            .ok_or_else(|| PyroscopeError::new("Sampler: Failed to unwrap sampler thread"))?
            .join()
            .map_err(|e| {
                PyroscopeError::new(
                    format!("Sampler: Failed to join sampler thread: {:?}", e).as_str(),
                )
            })?;
        Ok(())
    }
}
