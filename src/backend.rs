use ocaml::Runtime;
use pyroscope::{
    backend::{Backend, BackendConfig, Report, Rule, Ruleset, StackBuffer, StackFrame, StackTrace},
    error::Result,
    PyroscopeError,
};
use std::{
    cell::RefCell,
    ops::Deref,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex, OnceLock,
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
    static OCAML_CURSOR: OnceLock<ocaml_intf::Cursor> = OnceLock::new();
}

#[derive(Debug, Clone)]
pub struct CamlSpyConfig {
    pub sample_rate: u32,
    pub pid: u32,
    pub event_directory: PathBuf,
}

impl CamlSpyConfig {
    fn get_event_file_path(&self) -> PathBuf {
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
}

#[derive(Debug)]
pub struct CamlSpy {
    buffer: Arc<Mutex<StackBuffer>>,
    running: Arc<AtomicBool>,
    ruleset: Arc<Mutex<Ruleset>>,
    backend_config: Arc<Mutex<BackendConfig>>,
    config: CamlSpyConfig,
    sampler_thread: Option<JoinHandle<Result<()>>>,
}

impl CamlSpy {
    pub fn new(config: CamlSpyConfig, backend_config: BackendConfig) -> Self {
        CamlSpy {
            buffer: Arc::new(Mutex::new(StackBuffer::default())),
            running: Arc::new(AtomicBool::new(false)),
            ruleset: Arc::new(Mutex::new(Ruleset::default())),
            backend_config: Arc::new(Mutex::new(backend_config)),
            config,
            sampler_thread: None,
        }
    }
}

impl Backend for CamlSpy {
    fn spy_name(&self) -> Result<String> {
        Ok("camlspy".to_string())
    }

    fn spy_extension(&self) -> Result<Option<String>> {
        Ok(Some("cpu".to_string()))
    }

    fn sample_rate(&self) -> Result<u32> {
        log::debug!(target:LOG_TAG, "request sample rate");
        Ok(self.config.sample_rate)
    }

    fn initialize(&mut self) -> Result<()> {
        self.running.store(true, Ordering::Relaxed);
        let running = self.running.clone();
        let ruleset = self.ruleset.clone();
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
            Some(config.pid as u32),
            None,
            None,
            vec![empty_frame],
        );
        let sampler = thread::spawn(move || {
            log::debug!(target:LOG_TAG, "starting sampler thread");
            while running.load(Ordering::Relaxed) {
                log::trace!(target:LOG_TAG, "sampling...");
                let backend_config = backend_config.lock()?;
                let mut stack_frames = OCAML_GC.with_borrow(|gc| {
                    ocaml_intf::read_poll(
                        gc,
                        config.acquire_cursor(),
                        backend_config.deref(),
                        config.pid,
                    )
                });
                let mut stack_frames = match stack_frames {
                    Ok(frames) => frames,
                    Err(e) => {
                        log::error!(target:LOG_TAG, "failed to read poll: {}", e);
                        vec![]
                    }
                };

                if stack_frames.is_empty() {
                    // push an empty stack_trace to indicate idle
                    log::trace!(target:LOG_TAG, "no stack frames found, pushing empty stack trace");
                    stack_frames.push(empty_stack_trace.clone());
                }

                log::trace!(target:LOG_TAG, "got {} stack frames", stack_frames.clone().len());
                for st in stack_frames.into_iter() {
                    let stack_trace = st + &ruleset.lock()?.clone();
                    buffer.lock()?.record(stack_trace).unwrap();
                }
                thread::sleep(std::time::Duration::from_millis(
                    1000 / config.sample_rate as u64,
                ));
            }
            Ok(())
        });
        self.sampler_thread = Some(sampler);
        Ok(())
    }

    fn shutdown(self: Box<Self>) -> Result<()> {
        self.running.store(false, Ordering::Relaxed);
        self.sampler_thread
            .ok_or_else(|| PyroscopeError::new("CamlSpy: Failed to unwrap sampler thread"))?
            .join()
            .unwrap_or_else(|_| {
                Err(PyroscopeError::new(
                    "CamlSpy: Failed to join sampler thread",
                ))
            })?;
        Ok(())
    }

    fn report(&mut self) -> Result<Vec<Report>> {
        let report: StackBuffer = self.buffer.lock()?.deref().to_owned();
        let reports: Vec<Report> = report.into();

        self.buffer.lock()?.clear();
        Ok(reports)
    }

    fn add_rule(&self, rule: Rule) -> Result<()> {
        self.ruleset.lock()?.add_rule(rule)?;

        Ok(())
    }

    fn remove_rule(&self, rule: Rule) -> Result<()> {
        self.ruleset.lock()?.remove_rule(rule)?;

        Ok(())
    }

    fn set_config(&self, config: BackendConfig) {
        let mut backend_config = self.backend_config.lock().unwrap();
        *backend_config = config;
    }

    fn get_config(&self) -> Result<BackendConfig> {
        Ok(self.backend_config.lock().unwrap().deref().clone())
    }
}
