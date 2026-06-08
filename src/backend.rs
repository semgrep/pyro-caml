use ocaml::Runtime;
use pyroscope::{
    PyroscopeError, backend::{Backend, BackendConfig, Report, ReportBatch, ReportData, StackBuffer, StackFrame, StackTrace, ThreadTag, ThreadTagsSet}, error::Result
};
use std::{
    cell::RefCell,
    ops::Deref,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex, MutexGuard, OnceLock,
    },
    thread::{self, JoinHandle},
};

use crate::ocaml_intf;

const LOG_TAG: &str = "Pyro_caml::backend::camlspy_backend";
// Runtime doesn't implement send, it is really unstable across threads if
// it's been initialized in one but not the other due to how ocaml domain
// locks are implemented. So we cheat here and use a thread local runtime
thread_local! {
    static OCAML_GC: RefCell<Runtime> =  RefCell::new(ocaml::init()) ;
    static OCAML_CURSOR: OnceLock<ocaml_intf::Cursor> = const { OnceLock::new() };
}

#[derive(Debug, Clone)]
pub struct CamlSpyConfig {
    pub sample_rate: u32,
    pub pid: u32,
    pub event_directory: PathBuf,
}

impl CamlSpyConfig {
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
}

#[derive(Debug)]
pub struct CamlSpy {
    buffer: Arc<Mutex<StackBuffer>>,
    running: Arc<AtomicBool>,
    tagset: Arc<Mutex<ThreadTagsSet>>,
    backend_config: Arc<Mutex<BackendConfig>>,
    config: CamlSpyConfig,
    sampler_thread: Option<JoinHandle<()>>,
}

impl CamlSpy {
    pub fn new(config: CamlSpyConfig, backend_config: BackendConfig) -> Self {
        CamlSpy {
            buffer: Arc::new(Mutex::new(StackBuffer::default())),
            running: Arc::new(AtomicBool::new(false)),
            tagset: Arc::new(Mutex::new(ThreadTagsSet::default())),
            backend_config: Arc::new(Mutex::new(backend_config)),
            config,
            sampler_thread: None,
        }
    }
}

impl Backend for CamlSpy {
    fn initialize(&mut self) -> Result<()> {
        self.running.store(true, Ordering::Relaxed);
        let running = self.running.clone();
        let tagset = self.tagset.clone();
        let backend_config = self.backend_config.clone();
        let buffer = self.buffer.clone();
        let config = self.config.clone();
        let empty_frame = StackFrame {
            module: None,
            name: Some("unknown".to_string()),
            filename: None,
            relative_path: None,
            absolute_path: None,
            line: None,
        };

        let empty_stack_trace = StackTrace::new(
            &self.backend_config.lock().unwrap(),
            Some(config.pid),
            None,
            None,
            vec![empty_frame],
        );
        let sampler = thread::spawn(move || {
            log::debug!(target:LOG_TAG, "starting sampler thread");
            while running.load(Ordering::Relaxed) {
                log::trace!(target:LOG_TAG, "acquiring backend config and cursor");
                let backend_config = backend_config
                    .lock()
                    .expect("Could not take backend config lock"); // we only have one sampler thread and one reporting thread so this should never fail unless something is seriously wrong

                let cursor = config.acquire_cursor();
                log::trace!(target:LOG_TAG, "sampling...");
                let mut stack_frames: Vec<StackTrace> = OCAML_GC
                    .with_borrow(|gc| ocaml_intf::read_poll(gc, cursor, config.sample_interval()))
                    .unwrap_or_else(|e| {
                        log::error!(target:LOG_TAG, "Error reading from OCaml runtime: {:?}", e);
                        vec![]
                    })
                    .into_iter()
                    .map(|st| st.into_stack_trace(backend_config.deref(), config.pid))
                    .collect();

                log::trace!(target:LOG_TAG, "done sampling");

                if stack_frames.is_empty() {
                    // push an empty stack_trace to indicate idle
                    log::trace!(target:LOG_TAG, "no stack frames found, pushing empty stack trace");
                    stack_frames.push(empty_stack_trace.clone());
                }

                log::trace!(target:LOG_TAG, "recording stack frames");
                for st in stack_frames.into_iter() {
                    let stack_trace = st.add_tag_rules(&tagset.lock().expect("Failed to lock ruleset"));
                    buffer
                        .lock()
                        .expect("Failed to lock buffer") // we only have one sampler thread and one sending reports so we should never fail to obtain the lock unless something is seriously wrong
                        .record(stack_trace)
                        .expect("Failed to record stack traces to buffer"); // So this seems to just be a result in the pyroscope sdk for no reason
                }
                log::trace!(target:LOG_TAG, "recorded stack trace");
                log::trace!(target:LOG_TAG, "sleeping for {} ms", 1000 / config.sample_rate);

                thread::sleep(std::time::Duration::from_millis(
                    1000 / config.sample_rate as u64,
                ));
            }
            log::debug!(target:LOG_TAG, "sampler thread exiting")
        });
        self.sampler_thread = Some(sampler);
        Ok(())
    }

    fn shutdown(self: Box<Self>) -> Result<()> {
        log::trace!(target:LOG_TAG, "Shutting down sampler thread");
        self.running.store(false, Ordering::Relaxed);
        self.sampler_thread
            .ok_or_else(|| PyroscopeError::new("CamlSpy: Failed to unwrap sampler thread"))?
            .join()
            .map_err(|e| {
                PyroscopeError::new(
                    format!("CamlSpy: Failed to join sampler thread: {:?}", e).as_str(),
                )
            })?;
        Ok(())
    }

    fn report(&mut self) -> Result<ReportBatch> {
        // first check if the thread has finished
        if let Some(handle) = &self.sampler_thread && handle.is_finished() {
            return Err(PyroscopeError::new(
                "CamlSpy: Sampler thread has exited unexpectedly",
            ));
        }
        let mut buffer: MutexGuard<'_, StackBuffer> = self.buffer.lock()?;
        let reports: Vec<Report> = buffer.deref().to_owned().into();
        buffer.clear();

        let report_batch: ReportBatch = ReportBatch { 
            profile_type: "process_cpu".to_string(),
            data: ReportData::Reports(reports)
        };
        
        Ok(report_batch)
    }

    fn add_tag(&self, tag: ThreadTag) -> Result<()> {
        self.tagset.lock()?.add(tag)?;

        Ok(())
    }

    fn remove_tag(&self, tag: ThreadTag) -> Result<()> {
        self.tagset.lock()?.remove(tag)?;

        Ok(())
    }
}
