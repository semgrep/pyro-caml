use std::{path::PathBuf, thread, time::Duration};

use crate::backend::{CamlSpy, CamlSpyConfig};
use clap::Parser;
use nix::{
    sys::signal::{self, Signal},
    unistd::Pid,
};
use pyroscope::{
    backend::{BackendConfig, BackendImpl},
    pyroscope::{PyroscopeAgentBuilder, ReportEncoding},
};
use tempdir::TempDir;

mod backend;
mod ocaml_intf;

const OCAML_RUNTIME_EVENTS_START: &str = "OCAML_RUNTIME_EVENTS_START";
const OCAML_RUNTIME_EVENTS_DIR: &str = "OCAML_RUNTIME_EVENTS_DIR";
const LOG_TAG: &str = "Pyro_caml::main";

#[derive(Parser)]
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

    /// How many times per second to sample the OCaml program
    #[arg(long = "rate", env = "PYRO_CAML_SAMPLE_RATE", default_value_t = 100)]
    sample_rate: u32,

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
fn string_to_tags<'a>(tags: &'a str) -> Vec<(&'a str, &'a str)> {
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
    server_address: &str,
    service_name: &str,
    tags: Vec<(&str, &str)>,
    basic_auth_username: Option<&str>,
    basic_auth_password: Option<&str>,
) -> PyroscopeAgentBuilder {
    let mut agent_builder = PyroscopeAgentBuilder::new(server_address, service_name);
    agent_builder = agent_builder
        .report_encoding(ReportEncoding::PPROF)
        // TODO: add some tags about pyro caml's version
        .tags(tags);
    // Optionally configure auth. localhost:4040 usually doesn't need it but
    // grafana does
    //
    // TODO token auth?
    match (basic_auth_username, basic_auth_password) {
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

fn main() {
    let cli = Cli::parse();

    // there is probably a better way to do this
    unsafe { std::env::set_var("RUST_LOG", cli.verbosity.log_level_filter().to_string()) };
    pretty_env_logger::init_timed();

    let bin = cli.binary;
    let args = cli.args;
    let sample_rate = cli.sample_rate;
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

    let mut agent_builder = make_agent_builder(
        &cli.server_address,
        &cli.service_name,
        string_to_tags(&cli.tags),
        cli.basic_auth_username.as_deref(),
        cli.basic_auth_password.as_deref(),
    );
    let backend_config = BackendConfig {
        report_thread_id: true,
        report_thread_name: true,
        // do we really care about this?
        report_pid: true,
        report_oncpu: true,
    };
    log::info!(target: LOG_TAG, "Starting child process: {:?} {:?}", bin, args.clone());
    // fork and call bin_path with args
    let mut child = std::process::Command::new(bin)
        .args(args)
        .env(OCAML_RUNTIME_EVENTS_START, "1")
        .env(OCAML_RUNTIME_EVENTS_DIR, event_directory.to_str().unwrap())
        .spawn()
        .expect("failed to execute process");
    let child_id = child.id();
    // wait for child process to start

    let camlspy_config = CamlSpyConfig {
        event_directory,
        pid: child.id(),
        sample_rate,
    };
    let backend = BackendImpl::new(
        Box::new(CamlSpy::new(camlspy_config.clone(), backend_config)),
        Some(backend_config),
    );
    agent_builder = agent_builder.backend(backend);
    let agent = agent_builder.build().unwrap();
    let agent_running = agent.start().unwrap();

    ctrlc::set_handler(move || {
        log::info!(target: LOG_TAG, "Received Ctrl-C, shutting down...");

        signal::kill(Pid::from_raw(child_id.clone() as i32), Signal::SIGTERM)
            .expect("Failed to send SIGTERM to child process");
    })
    .expect("Error setting Ctrl-C handler");

    let ecode = child.wait().expect("failed to wait on child");
    match ecode.success() {
        true => log::info!(target: LOG_TAG, "Process exited successfully"),
        false => {
            log::error!(target: LOG_TAG, "Process exited with exit code: {:?}", ecode);
            if !keep_events_file {
                log::info!(target: LOG_TAG, "Cleaning up events file: {:?}", camlspy_config.get_event_file_path());
                std::fs::remove_file(camlspy_config.get_event_file_path()).unwrap_or_else(
                    |err| log::error!(target: LOG_TAG, "Failed to remove events file: {:?}", err),
                );
            }
        }
    }

    // sleep for 1 seconds to allow the agent to flush data
    thread::sleep(Duration::from_secs(1));
    agent_running.stop().unwrap();
    // exit with the same code as the child process
    std::process::exit(ecode.code().unwrap_or_default());
}
