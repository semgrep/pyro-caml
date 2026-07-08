use std::{
    path::PathBuf,
    sync::{Arc, Mutex, atomic::AtomicBool},
    thread,
    time::Duration,
};

use crate::{
    backend::CamlSpy,
    sampler::{ProfileBuffer, Sampler, SamplerConfig},
};
use clap::Parser;
use nix::{
    sys::signal::{self, Signal},
    unistd::Pid,
};
use pyroscope::{
    PyroscopeAgent,
    backend::{BackendConfig, BackendImpl, BackendUninitialized, ThreadTagsSet},
    encode::pprof::ProfilingType::{AllocObjects, AllocSpace, Cpu, InuseObjects, InuseSpace},
    pyroscope::{PyroscopeAgentBuilder, PyroscopeAgentRunning},
};
use tempdir::TempDir;

mod backend;
mod ocaml_intf;
mod sampler;

const OCAML_RUNTIME_EVENTS_START: &str = "OCAML_RUNTIME_EVENTS_START";
const OCAML_RUNTIME_EVENTS_DIR: &str = "OCAML_RUNTIME_EVENTS_DIR";
const OCAML_MEMPROF_SAMPLING_RATE: &str = "OCAML_MEMPROF_SAMPLING_RATE";
const LOG_TAG: &str = "Pyro_caml::main";

#[derive(Parser, Clone)]
#[command(version, about="An OCaml profiler compatible with Pyroscope", long_about = None)]
struct Cli {
    /// Name of the service that is being profiled
    #[arg(long = "name", env = "PYRO_CAML_SERVICE_NAME")]
    service_name: String,

    /// Pyroscope server that profiles will be sent to
    #[arg(
        long = "address",
        env = "PYRO_CAML_SERVER_ADDRESS",
        default_value = "http://localhost:4040"
    )]
    server_address: String,

    /// Username for authorization with the Pyroscope server
    #[arg(long = "username", env = "PYRO_CAML_BASIC_AUTH_USERNAME")]
    basic_auth_username: Option<String>,

    ///Password for authorization with the Pyroscope server
    #[arg(long = "password", env = "PYRO_CAML_BASIC_AUTH_PASSWORD")]
    basic_auth_password: Option<String>,

    /// Which directory the OCaml runtime events will store its temporary event files in
    #[arg(long = "event_directory", env = "PYRO_CAML_EVENT_DIRECTORY")]
    event_directory: Option<PathBuf>,

    /// How many times per second to sample the OCaml program for CPU profiling
    #[arg(long = "rate", env = "PYRO_CAML_SAMPLE_RATE", default_value_t = 100)]
    sample_rate: u32,

    /// Memprof sampling rate: the probability that any given allocated word is
    /// sampled. Passed through to Gc.Memprof.start in the profiled program.
    /// 1e-6 is nice but chosen somewhat randomly. Too high and you end up sending
    /// too many points and overwhelming the profiler, too little and you don't get
    /// enough info. Choose a higher value such as [1e-4] if the profiler sample rate is
    /// high, or not many allocations happen in your program. Alternatively if not
    /// many allocations happen, use {!emit_point_event} or the Pyro Caml PPX.
    #[arg(
        long = "memprof_rate",
        env = "PYRO_CAML_MEMPROF_SAMPLING_RATE",
        default_value_t = 1e-7
    )]
    memprof_sampling_rate: f64,

    /// Max difference between sample time and sample, in microseconds,
    /// defaults to sample interval
    #[arg(
        long = "max_delta",
        env = "PYRO_CAML_MAX_DELTA",
        default_value_t = 10_000
    )]
    max_delta: u32,

    /// Tags to attach to the profiles, in the format key1=value1,key2=value2
    #[arg(long = "tags", env = "PYRO_CAML_TAGS", default_value = "")]
    tags: String,

    /// Whether to keep the OCaml runtime events file after the profiled program exits
    #[arg(
        long = "keep_events_file",
        env = "PYRO_CAML_KEEP_EVENTS_FILE",
        default_value_t = false
    )]
    keep_events_file: bool,

    #[command(flatten)]
    verbosity: clap_verbosity_flag::Verbosity,

    /// Name of the OCaml binary to profile
    binary: PathBuf,

    /// Arguments to pass to BINARY
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    args: Vec<String>,
}
// Convert a string of tags to a Vec<(&str, &str)>, so we can parse the tags
// config value
fn string_to_tags(tags: &str) -> Vec<(&str, &str)> {
    let mut tags_vec = Vec::new();

    // check if string is empty
    if tags.is_empty() {
        return tags_vec;
    }

    for tag in tags.split(',') {
        let mut tag_split = tag.split('=');
        let key = tag_split.next().unwrap();
        let value = tag_split.next().unwrap();
        tags_vec.push((key, value));
    }

    tags_vec
}
fn make_agent_builder(
    cli: Cli,
    profile: &ProfileBuffer,
    backend: BackendImpl<BackendUninitialized>,
) -> PyroscopeAgentBuilder {
    let mut agent_builder = PyroscopeAgentBuilder::new(
        cli.server_address,
        cli.service_name,
        cli.sample_rate,
        "camlspy",
        env!("CARGO_PKG_VERSION"),
        backend,
    );
    agent_builder = agent_builder
        // TODO: add some tags about pyro caml's version
        .tags(string_to_tags(&cli.tags))
        .upload_interval(profile.upload_interval)
        .profiling_type(profile.profiling_type);
    // Optionally configure auth. localhost:4040 usually doesn't need it but
    // grafana does
    //
    // TODO token auth?
    match (
        cli.basic_auth_username.as_deref(),
        cli.basic_auth_password.as_deref(),
    ) {
        (Some(username), Some(password)) => {
            log::info!(target: LOG_TAG, "Using basic auth with username: {}", username);
            agent_builder = agent_builder.basic_auth(username, password);
        }
        (Some(_), None) | (None, Some(_)) => {
            log::warn!(target: LOG_TAG, "One but not both of basic auth username or password not provided, skipping basic auth setup");
        }
        (None, None) => {}
    };
    agent_builder
}
fn make_agent_running(
    profile: &ProfileBuffer,
    tagset: Arc<Mutex<ThreadTagsSet>>,
    alive: Arc<AtomicBool>,
    cli: Cli,
) -> PyroscopeAgent<PyroscopeAgentRunning> {
    let backend = BackendImpl::new(Box::new(CamlSpy::new(
        profile.buffer.clone(),
        tagset,
        alive,
        profile.profiling_type,
    )));

    let agent_builder = make_agent_builder(cli, profile, backend);
    let agent = agent_builder.build().unwrap();
    agent.start().unwrap()
}

fn main() {
    let cli = Cli::parse();
    let agent_cli = cli.clone();

    pretty_env_logger::formatted_timed_builder()
        .filter_level(cli.verbosity.log_level_filter())
        .init();

    let bin = cli.binary;
    let args = cli.args;
    let sample_rate = cli.sample_rate;
    let memprof_rate = cli.memprof_sampling_rate;
    let max_delta = cli.max_delta;
    let event_directory = cli.event_directory.unwrap_or_else(|| {
        // use a temp dir since that'll probably be in memory and probably faster
        let dir = TempDir::new("pyro_caml")
            .expect("failed to create temp dir")
            .into_path();
        log::debug!(target: LOG_TAG, "Using temporary event directory: {:?}", dir);
        dir
    });
    let keep_events_file = cli.keep_events_file;

    log::info!(target: LOG_TAG, "Sending profiles to Pyroscope server at: {}", cli.server_address);
    log::info!(target: LOG_TAG, "Starting child process: {:?} {:?}", bin, args.clone());
    // fork and call bin_path with args
    let mut child = std::process::Command::new(bin)
        .args(args)
        .env(OCAML_RUNTIME_EVENTS_START, "1")
        .env(OCAML_RUNTIME_EVENTS_DIR, event_directory.to_str().unwrap())
        .env(
            OCAML_MEMPROF_SAMPLING_RATE,
            cli.memprof_sampling_rate.to_string(),
        )
        .spawn()
        .expect("failed to execute process");
    let child_id = child.id();
    // wait for child process to start

    let sampler_config = SamplerConfig {
        event_directory,
        pid: child.id(),
        sample_rate,
        memprof_rate,
        max_delta,
    };
    let tagset = Arc::new(Mutex::new(ThreadTagsSet::default()));

    let backend_config = Arc::new(Mutex::new(BackendConfig {
        report_thread_id: true,
        report_thread_name: true,
        // do we really care about this?
        report_pid: true,
    }));

    let alive = Arc::new(AtomicBool::new(false));

    // The single source of truth for which profiles run. Each owns a StackBuffer
    // that the sampler writes into (shared via Arc) and the agent drains when
    // reporting. To add a profile (inuse_space, alloc_objects, ...), add its
    // profile type here and the matching arm in ProfileBuffer::record.
    let profiles: Vec<ProfileBuffer> = [Cpu, AllocSpace, AllocObjects, InuseSpace, InuseObjects]
        .into_iter()
        .map(ProfileBuffer::new)
        .collect();

    let agents: Vec<PyroscopeAgent<PyroscopeAgentRunning>> = profiles
        .iter()
        .map(|profile| {
            make_agent_running(profile, tagset.clone(), alive.clone(), agent_cli.clone())
        })
        .collect();

    let sampler = Sampler::start(
        sampler_config.clone(),
        backend_config,
        tagset.clone(),
        alive.clone(),
        profiles,
    );

    ctrlc::set_handler(move || {
        log::info!(target: LOG_TAG, "Received Ctrl-C, shutting down...");

        signal::kill(Pid::from_raw(child_id as i32), Signal::SIGTERM)
            .expect("Failed to send SIGTERM to child process");
    })
    .expect("Error setting Ctrl-C handler");

    let ecode = child.wait().expect("failed to wait on child");
    match ecode.success() {
        true => log::info!(target: LOG_TAG, "Process exited successfully"),
        false => {
            log::error!(target: LOG_TAG, "Process exited with exit code: {:?}", ecode);
            if !keep_events_file {
                log::info!(target: LOG_TAG, "Cleaning up events file: {:?}", sampler_config.get_event_file_path());
                std::fs::remove_file(sampler_config.get_event_file_path()).unwrap_or_else(
                    |err| log::error!(target: LOG_TAG, "Failed to remove events file: {:?}", err),
                );
            }
        }
    }

    // sleep for 1 seconds to allow the agent to flush data
    thread::sleep(Duration::from_secs(1));
    // Stop every agent (and the sampler) before unwrapping any result, so one
    // agent's flush failure doesn't leak the others or skip sampler shutdown.
    let agent_results: Vec<_> = agents.into_iter().map(|agent| agent.stop()).collect();
    let sampler_result = sampler.stop();
    for result in agent_results {
        result.unwrap();
    }
    sampler_result.unwrap();

    // exit with the same code as the child process
    std::process::exit(ecode.code().unwrap_or_default());
}
