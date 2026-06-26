use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::Deref,
    path::{Path, PathBuf},
    sync::{
        Arc, Mutex, OnceLock,
        atomic::{AtomicBool, Ordering},
        mpsc::{
            self, Receiver,
            RecvTimeoutError::{Disconnected, Timeout},
            Sender,
        },
    },
    thread::{self, JoinHandle},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use ocaml::Runtime;
use pyroscope::{
    PyroscopeError,
    backend::{BackendConfig, StackBuffer, StackFrame, StackTrace, ThreadTagsSet},
    encode::pprof::ProfilingType,
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
    pub memprof_rate: f64,
    pub max_delta: u32,
    pub pid: u32,
    pub event_directory: PathBuf,
}

/// Sampler preprocesses and records relevant samples into the [StackBuffer]
/// based on the [ProfilingType]. The buffer is shared with the backend agent
/// which sends the stack traces to the Pyroscope server for visualization.
/// live maps block id to its stack trace and either the number of bytes or
/// the object count.
#[derive(Clone)]
pub struct ProfileBuffer {
    pub profiling_type: ProfilingType,
    pub buffer: Arc<Mutex<StackBuffer>>,
    live: Arc<Mutex<HashMap<i64, (StackTrace, usize)>>>,
    pub upload_interval: Duration,
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum PointKind {
    Alloc,
    Dealloc,
}

impl PointKind {
    fn from_ocaml(kind: isize) -> Self {
        match kind {
            1 => PointKind::Dealloc,
            _ => PointKind::Alloc,
        }
    }
}

#[derive(Clone)]
struct SamplePoint {
    time: f64,
    stack_trace: StackTrace,
    n_samples: usize,
    size: usize,
    kind: PointKind,
    id: i64,
}

/// A [SamplePoint] whose stack trace has already had thread-tag rules applied.
/// Every backend records the same tagged traces, so tagging is done once per
/// tick (see [Sampler::tag_samples]) rather than once per backend.
struct TaggedSample {
    time: f64,
    stack_trace: StackTrace,
    n_samples: usize,
    size: usize,
    kind: PointKind,
    id: i64,
}

pub struct Sampler {
    running: Arc<AtomicBool>,
    /// The drain thread (owns the OCaml runtime + cursor; does only read_poll)
    /// and the processing thread (tags + records; touches no OCaml).
    threads: Vec<JoinHandle<()>>,
}

impl Sampler {
    /// Sampler takes in a [ProfileBuffer] per profile type. It drains the OCaml
    /// event buffer and fans each sample out into every profile's buffer. This
    /// makes it possible to support multiple different profiles simultaneously
    /// (cpu, alloc_space, inuse_space, etc.) — adding one is a single
    /// [ProfileBuffer] entry.
    ///
    /// We do the processing for each profile here because we sometimes
    /// want to pass in the sample time such as in cpu usage
    pub fn start(
        config: SamplerConfig,
        backend_config: Arc<Mutex<BackendConfig>>,
        tagset: Arc<Mutex<ThreadTagsSet>>,
        alive: Arc<AtomicBool>,
        profiles: Vec<ProfileBuffer>,
    ) -> Self {
        let running = Arc::new(AtomicBool::new(true));
        alive.store(true, Ordering::Relaxed);

        // Split sampling into two threads. drain_thread calls read_poll and
        // moves the samples into an unbounded rust heap channel so that we
        // can drain the fixed-size ring buffer quickly. processing_thread
        // does all the processing and recording into profile buffers.
        let (tx, rx) = mpsc::channel::<(f64, Vec<SamplePoint>)>();

        let drain_thread = Self::spawn_drain_thread(
            alive,
            tx,
            running.clone(),
            config.clone(),
            backend_config.clone(),
        );

        let processing_thread = Self::spawn_processing_thread(
            rx,
            running.clone(),
            tagset,
            config,
            backend_config,
            profiles,
        );

        Sampler {
            running,
            threads: vec![drain_thread, processing_thread],
        }
    }

    /// Owns the OCaml runtime + cursor, doing only read_poll + decode, then
    /// hands the resolved samples off over a channel. Keeping it cheap lets
    /// the per-domain runtime_events rings be drained on a regular cadence
    /// so they don't overflow and lose events.
    fn spawn_drain_thread(
        alive: Arc<AtomicBool>,
        tx: Sender<(f64, Vec<SamplePoint>)>,
        drain_running: Arc<AtomicBool>,
        drain_config: SamplerConfig,
        drain_backend_config: Arc<Mutex<BackendConfig>>,
    ) -> JoinHandle<()> {
        thread::spawn(move || {
            // The drain thread is the irreplaceable one (it owns the OCaml
            // runtime), so it carries the liveness guard: if it dies,
            // CamlSpy::report sees `alive == false` and surfaces the error.
            let _alive_guard = AliveGuard(alive);
            log::debug!(target:LOG_TAG, "starting sampler drain thread");
            while drain_running.load(Ordering::Relaxed) {
                let cursor = drain_config.acquire_cursor();
                log::trace!(target:LOG_TAG, "draining ring...");
                let output = OCAML_GC
                    .with_borrow(|gc| ocaml_intf::read_poll(gc, cursor))
                    .unwrap_or_else(|e| {
                        log::error!(target:LOG_TAG, "Error reading from OCaml runtime: {:?}", e);
                        // Fall back to the Rust clock so age-based filtering in
                        // record_cpu still has a sane reference time.
                        ocaml_intf::ReadPollOutput {
                            now: SystemTime::now()
                                .duration_since(UNIX_EPOCH)
                                .expect("system clock before unix epoch")
                                .as_secs_f64(),
                            sample_points: vec![],
                        }
                    });
                let now: f64 = output.now;
                let sample_points: Vec<SamplePoint> = {
                    let backend_config = drain_backend_config
                        .lock()
                        .expect("Could not take backend config lock");
                    output
                        .sample_points
                        .into_iter()
                        .map(|csp| SamplePoint {
                            time: csp.time,
                            stack_trace: csp
                                .stack_trace
                                .into_stack_trace(backend_config.deref(), drain_config.pid),
                            n_samples: usize::try_from(csp.n_samples).unwrap(),
                            size: usize::try_from(csp.size).unwrap(),
                            kind: PointKind::from_ocaml(csp.kind),
                            id: csp.id as i64,
                        })
                        .collect()
                };

                // mpsc::channel is an async channel and send will never block
                // this thread because it has "infinite buffer" unlike sync_channel.
                if let Err(e) = tx.send((now, sample_points)) {
                    // The processing thread should only stop before the drain thread during shutdown
                    // so we log the error if we are still running
                    if drain_running.load(Ordering::Relaxed) {
                        log::error!(target:LOG_TAG, "processing thread disconnected unexpectedly while still running ({}); drain thread exiting", e);
                    } else {
                        log::debug!(target:LOG_TAG, "processing thread stopped during shutdown; drain thread exiting");
                    }
                    break;
                }

                thread::sleep(std::time::Duration::from_millis(
                    1000 / drain_config.sample_rate as u64,
                ));
            }
            log::debug!(target:LOG_TAG, "sampler drain thread exiting")
        })
    }

    /// Receives each samples and does heavier tagging, preprocessing, and
    /// recording into each profile
    fn spawn_processing_thread(
        rx: Receiver<(f64, Vec<SamplePoint>)>,
        processing_running: Arc<AtomicBool>,
        tagset: Arc<Mutex<ThreadTagsSet>>,
        config: SamplerConfig,
        backend_config: Arc<Mutex<BackendConfig>>,
        profiles: Vec<ProfileBuffer>,
    ) -> JoinHandle<()> {
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

        thread::spawn(move || {
            log::debug!(target:LOG_TAG, "starting sampler processing thread");
            while processing_running.load(Ordering::Relaxed) {
                // recv_timeout blocks while waiting for samples from the Sender in the other thread
                // until it times out every 100ms to check whether we're still running. This is so
                // we can stop the thread if the profiled program finishes either on its own or via
                // SIGTERM. It unblocks when a message is received from the Sender and processes the
                // samples in the message.
                let (now, mut sample_points) = match rx
                    .recv_timeout(std::time::Duration::from_millis(100))
                {
                    Ok(batch) => batch,
                    Err(Timeout) => {
                        log::warn!(target:LOG_TAG, "timed out waiting for samples from drain thread");
                        continue;
                    }
                    Err(Disconnected) => {
                        if processing_running.load(Ordering::Relaxed) {
                            log::error!(target:LOG_TAG, "drain thread disconnected unexpectedly while still running; processing thread exiting");
                        } else {
                            log::debug!(target:LOG_TAG, "drain thread stopped during shutdown; processing thread exiting");
                        }
                        break;
                    }
                };

                if sample_points.is_empty() {
                    log::trace!(target:LOG_TAG, "no stack frames found, pushing empty stack trace");
                    sample_points.push(SamplePoint {
                        time: now,
                        stack_trace: empty_stack_trace.clone(),
                        n_samples: 0,
                        size: 0,
                        kind: PointKind::Alloc,
                        id: 0,
                    });
                }

                // Tag once, then fan the tagged samples out to every profile.
                let tagged = Self::tag_samples(sample_points, &tagset);
                for profile in &profiles {
                    profile.record(&tagged, now, &config);
                }
            }
            log::debug!(target:LOG_TAG, "sampler processing thread exiting")
        })
    }

    /// Apply thread-tag rules to every sample once, under a single tagset lock.
    /// Every backend records the same tagged traces, so there is no reason to
    /// run the (identical) rule matching, or take the tagset lock, per backend.
    fn tag_samples(
        sample_points: Vec<SamplePoint>,
        tagset: &Mutex<ThreadTagsSet>,
    ) -> Vec<TaggedSample> {
        let tagset = tagset.lock().expect("Failed to lock ruleset");
        sample_points
            .into_iter()
            .map(|sp| TaggedSample {
                time: sp.time,
                stack_trace: sp.stack_trace.add_tag_rules(&tagset),
                n_samples: sp.n_samples,
                size: sp.size,
                kind: sp.kind,
                id: sp.id,
            })
            .collect()
    }

    pub fn stop(self) -> Result<()> {
        log::trace!(target:LOG_TAG, "Shutting down sampler threads");
        self.running.store(false, Ordering::Relaxed);
        for thread in self.threads {
            thread.join().map_err(|e| {
                PyroscopeError::new(
                    format!("Sampler: Failed to join sampler thread: {:?}", e).as_str(),
                )
            })?;
        }
        Ok(())
    }
}

impl ProfileBuffer {
    pub fn new(profiling_type: ProfilingType) -> Self {
        ProfileBuffer {
            profiling_type,
            buffer: Arc::new(Mutex::new(StackBuffer::default())),
            live: Arc::new(Mutex::new(HashMap::new())),
            // inuse_space and inuse_objects use a 15s upload interval to avoid
            // the problem of pyroscope aggregating adjacent points together in
            // 10s buckets and taking their sum instead of their average. The
            // other metrics are set at various prime number upload
            // intervals to avoid overwhelming the server with uploads that are
            // too large
            upload_interval: Duration::from_secs(match profiling_type {
                ProfilingType::Cpu => 11,
                ProfilingType::AllocSpace => 7,
                ProfilingType::AllocObjects => 7,
                ProfilingType::InuseSpace => 15,
                ProfilingType::InuseObjects => 15,
            }),
        }
    }

    /// Record this tick's already-tagged samples into this profile's buffer.
    /// Each profile owns a separate buffer, so it clones the traces it records.
    /// The match is exhaustive on purpose: adding a [ProfilingType] becomes a
    /// compile error here rather than a silently-unrecorded profile.
    fn record(&self, tagged: &[TaggedSample], now: f64, config: &SamplerConfig) {
        match self.profiling_type {
            ProfilingType::Cpu => self.record_cpu(tagged, now, config),
            ProfilingType::AllocSpace => self.record_alloc_space(tagged, config),
            ProfilingType::AllocObjects => self.record_alloc_objects(tagged, config),
            ProfilingType::InuseSpace => self.record_inuse_space(tagged, config),
            ProfilingType::InuseObjects => self.record_inuse_objects(tagged, config),
        }
    }

    fn record_cpu(&self, tagged: &[TaggedSample], now: f64, config: &SamplerConfig) {
        log::trace!(target:LOG_TAG, "processing stack frames for cpu buffer");
        let max_age = f64::min(config.sample_interval(), config.max_delta());

        // Keep the sample closest to `now` for each thread, within the staleness
        // window. We work over references and only clone the survivors, rather
        // than the whole sample set.
        let mut survivors: Vec<&TaggedSample> = tagged
            .iter()
            .filter(|s| s.kind == PointKind::Alloc && (now - s.time).abs() < max_age)
            .collect();
        survivors.sort_by(|a, b| (now - a.time).abs().total_cmp(&(now - b.time).abs()));

        let mut seen = HashSet::new();
        survivors.retain(|s| seen.insert(s.stack_trace.thread_id.clone()));

        log::trace!(target:LOG_TAG, "recording stack frames for cpu buffer");
        let mut buffer = self.buffer.lock().expect("Failed to lock buffer"); // we only have one sampler thread and one sending reports so we should never fail to obtain the lock unless something is seriously wrong
        for s in survivors {
            buffer
                .record(s.stack_trace.clone())
                .expect("Failed to record stack traces to buffer"); // So this seems to just be a result in the pyroscope sdk for no reason
        }
        log::trace!(target:LOG_TAG, "recorded stack trace for cpu buffer");
    }

    /// Estimated bytes for one sample, shared by alloc_space and inuse_space.
    /// memprof reports n_samples ~ Poisson(memprof_rate * size_in_words) per
    /// allocation, so n_samples / memprof_rate is an unbiased estimate of the
    /// allocation size in words. We convert words to bytes with the native word
    /// size.
    fn sample_bytes(n_samples: usize, config: &SamplerConfig) -> usize {
        let bytes_per_word = size_of::<usize>() as f64;
        (n_samples as f64 * bytes_per_word / config.memprof_rate).round() as usize
    }

    /// Estimated object count for one sample, shared by alloc_objects and
    /// inuse_objects. Returns 0 for samples that contribute nothing (size 0, or
    /// a sub-unit estimate that rounds to 0). We adjust for the fact that larger
    /// objects are more likely to be sampled; size + 1 is because the size
    /// memprof gives us excludes the header, even though the header can be
    /// sampled.
    fn sample_objects(n_samples: usize, size: usize, config: &SamplerConfig) -> usize {
        if size == 0 {
            return 0;
        }
        (n_samples as f64 / (((size + 1) as f64) * config.memprof_rate)).round() as usize
    }

    /// Helper function to record alloc_space and alloc_objects
    fn record_alloc(&self, tagged: &[TaggedSample], scale: impl Fn(&TaggedSample) -> usize) {
        let mut buffer = self.buffer.lock().expect("Failed to lock buffer"); // we only have one sampler thread and one sending reports so we should never fail to obtain the lock unless something is seriously wrong
        for s in tagged {
            // Skip Dealloc points
            if s.kind != PointKind::Alloc {
                continue;
            }
            let scaled_count = scale(s);
            if scaled_count == 0 {
                continue;
            }
            buffer
                .record_with_count(s.stack_trace.clone(), scaled_count)
                .expect("Failed to record stack traces to buffer"); // So this seems to just be a result in the pyroscope sdk for no reason
        }
    }

    fn record_alloc_space(&self, tagged: &[TaggedSample], config: &SamplerConfig) {
        log::trace!(target:LOG_TAG, "recording stack frames for alloc_space buffer");
        self.record_alloc(tagged, |s: &TaggedSample| {
            Self::sample_bytes(s.n_samples, config)
        });
        log::trace!(target:LOG_TAG, "recorded stack trace for alloc_space buffer");
    }

    fn record_alloc_objects(&self, tagged: &[TaggedSample], config: &SamplerConfig) {
        log::trace!(target:LOG_TAG, "recording stack frames for alloc_objects buffer");
        self.record_alloc(tagged, |s| {
            Self::sample_objects(s.n_samples, s.size, config)
        });
        log::trace!(target:LOG_TAG, "recorded stack trace for alloc_objects buffer");
    }

    /// Fold this tick's samples into the persistent live-block map by inserting
    /// Allocs and removing Deallocs, then rebuild the buffer as the current snapshot.
    fn record_inuse(&self, tagged: &[TaggedSample], scale: impl Fn(&TaggedSample) -> usize) {
        let mut live = self.live.lock().expect("Failed to lock live map");
        for s in tagged {
            match s.kind {
                PointKind::Alloc => {
                    let amount = scale(s);
                    if amount == 0 {
                        continue;
                    }
                    live.insert(s.id, (s.stack_trace.clone(), amount));
                }
                PointKind::Dealloc => {
                    live.remove(&s.id);
                }
            }
        }

        // Sum the still-live blocks per call site into the snapshot.
        let mut totals: HashMap<StackTrace, usize> = HashMap::new();
        for (stack_trace, amount) in live.values() {
            *totals.entry(stack_trace.clone()).or_insert(0) += *amount;
        }

        let mut buffer = self.buffer.lock().expect("Failed to lock buffer");
        buffer.clear();
        for (stack_trace, live_count) in totals {
            buffer
                .record_with_count(stack_trace, live_count)
                .expect("Failed to record stack traces to buffer");
        }
    }

    fn record_inuse_space(&self, tagged: &[TaggedSample], config: &SamplerConfig) {
        log::trace!(target:LOG_TAG, "recording stack frames for inuse_space buffer");
        self.record_inuse(tagged, |s| Self::sample_bytes(s.n_samples, config));
        log::trace!(target:LOG_TAG, "recorded stack trace for inuse_space buffer");
    }

    fn record_inuse_objects(&self, tagged: &[TaggedSample], config: &SamplerConfig) {
        log::trace!(target:LOG_TAG, "recording stack frames for inuse_objects buffer");
        self.record_inuse(tagged, |s| {
            Self::sample_objects(s.n_samples, s.size, config)
        });
        log::trace!(target:LOG_TAG, "recorded stack trace for inuse_objects buffer");
    }
}
