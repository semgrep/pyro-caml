use pyroscope::{
    PyroscopeError,
    backend::{Backend, Report, ReportBatch, ReportData, StackBuffer, ThreadTag, ThreadTagsSet},
    error::Result,
};
use std::{
    ops::Deref,
    sync::{
        Arc, Mutex, MutexGuard,
        atomic::{AtomicBool, Ordering},
    },
};

#[derive(Debug)]
pub struct CamlSpy {
    buffer: Arc<Mutex<StackBuffer>>,
    tagset: Arc<Mutex<ThreadTagsSet>>,
    alive: Arc<AtomicBool>,
}

impl CamlSpy {
    pub fn new(
        buffer: Arc<Mutex<StackBuffer>>,
        tagset: Arc<Mutex<ThreadTagsSet>>,
        alive: Arc<AtomicBool>,
    ) -> Self {
        CamlSpy {
            buffer,
            tagset,
            alive,
        }
    }
}

impl Backend for CamlSpy {
    fn initialize(&mut self) -> Result<()> {
        Ok(())
    }

    fn shutdown(self: Box<Self>) -> Result<()> {
        Ok(())
    }

    fn report(&mut self) -> Result<ReportBatch> {
        // first check if the sampler thread has finished
        if !self.alive.load(Ordering::Relaxed) {
            return Err(PyroscopeError::new(
                "CamlSpy: Sampler thread has exited unexpectedly",
            ));
        }
        let mut buffer: MutexGuard<'_, StackBuffer> = self.buffer.lock()?;
        let reports: Vec<Report> = buffer.deref().to_owned().into();
        buffer.clear();

        let report_batch: ReportBatch = ReportBatch {
            profile_type: "process_cpu".to_string(),
            data: ReportData::Reports(reports),
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
