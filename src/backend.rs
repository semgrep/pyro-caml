use pyroscope::{
    PyroscopeError,
    backend::{Backend, Report, ReportBatch, ReportData, StackBuffer, ThreadTag, ThreadTagsSet},
    encode::pprof::ProfilingType,
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
    profiling_type: ProfilingType,
}

impl CamlSpy {
    pub fn new(
        buffer: Arc<Mutex<StackBuffer>>,
        tagset: Arc<Mutex<ThreadTagsSet>>,
        alive: Arc<AtomicBool>,
        profiling_type: ProfilingType,
    ) -> Self {
        CamlSpy {
            buffer,
            tagset,
            alive,
            profiling_type,
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
        // Cumulative profiles (cpu, alloc_*) accumulate into the buffer between
        // reports, so each report must drain it — every upload is then a
        // disjoint per-interval slice. The inuse_* profiles are gauges: the
        // sampler rebuilds their buffer as a complete live snapshot every tick
        // (see ProfileBuffer::record_inuse), so report() must NOT clear them.
        if !matches!(
            self.profiling_type,
            ProfilingType::InuseSpace | ProfilingType::InuseObjects
        ) {
            buffer.clear();
        }

        let report_batch: ReportBatch = ReportBatch {
            profile_type: match self.profiling_type {
                ProfilingType::Cpu => "process_cpu".to_string(),
                ProfilingType::AllocSpace
                | ProfilingType::AllocObjects
                | ProfilingType::InuseSpace
                | ProfilingType::InuseObjects => "memory".to_string(),
            },
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
