use std::{env, path::PathBuf, thread, time::Duration};

use clap::Parser;
use pyroscope::{
    backend::{BackendConfig, BackendImpl},
    pyroscope::{PyroscopeAgentBuilder, ReportEncoding},
};

use crate::backend::{CamlSpy, CamlSpyConfig};

mod backend;
mod ocaml_intf;

const OCAML_RUNTIME_EVENTS_START: &str = "OCAML_RUNTIME_EVENTS_START";
const OCAML_RUNTIME_EVENTS_DIR: &str = "OCAML_RUNTIME_EVENTS_DIR";
const LOG_TAG: &str = "Pyro_caml::main";

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[arg(long = "address", env = "PYRO_CAML_SERVER_ADDRESS")]
    server_address: String,

    #[arg(long = "name", env = "PYRO_CAML_SERVICE_NAME")]
    service_name: String,

    #[arg(long = "username", env = "PYRO_CAML_BASIC_AUTH_USERNAME")]
    basic_auth_username: String,

    #[arg(long = "password", env = "PYRO_CAML_BASIC_AUTH_PASSWORD")]
    basic_auth_password: String,

    #[arg(long = "rate", env = "PYRO_CAML_SAMPLE_RATE", default_value_t = 100)]
    sample_rate: u32,

    #[arg(long = "event_directory", env = "PYRO_CAML_EVENT_DIRECTORY")]
    event_directory: Option<PathBuf>,

    #[command(flatten)]
    verbosity: clap_verbosity_flag::Verbosity,

    binary_path: PathBuf,

    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    args: Vec<String>,
}

fn make_agent_builder(
    server_address: &str,
    service_name: &str,
    basic_auth_username: &str,
    basic_auth_password: &str,
) -> PyroscopeAgentBuilder {
    let mut agent_builder = PyroscopeAgentBuilder::new(server_address, service_name);
    agent_builder = agent_builder
        .report_encoding(ReportEncoding::PPROF)
        .basic_auth(basic_auth_username, basic_auth_password);
    agent_builder
}

fn main() {
    let cli = Cli::parse();
    let bin_path = cli.binary_path;
    let args = cli.args;
    let sample_rate = cli.sample_rate;
    let event_directory = cli
        .event_directory
        .unwrap_or_else(|| env::current_dir().unwrap());

    unsafe { std::env::set_var("RUST_LOG", cli.verbosity.log_level_filter().to_string()) };
    pretty_env_logger::init_timed();

    // TODO get from flag or env
    let mut agent_builder = make_agent_builder(
        &cli.server_address,
        &cli.service_name,
        &cli.basic_auth_username,
        &cli.basic_auth_password,
    );
    let backend_config = BackendConfig {
        report_thread_id: true,
        report_thread_name: true,
        report_pid: true,
        report_oncpu: true,
    };
    log::info!(target: LOG_TAG, "Starting child process: {:?} {:?}", bin_path, args.clone());
    // fork and call bin_path with args
    let mut child = std::process::Command::new(bin_path)
        .args(args)
        .env(OCAML_RUNTIME_EVENTS_START, "1")
        .env(OCAML_RUNTIME_EVENTS_DIR, event_directory.to_str().unwrap())
        .spawn()
        .expect("failed to execute process");
    // wait for child process to start

    let camlspy_config = CamlSpyConfig {
        event_directory,
        pid: child.id(),
        sample_rate,
    };
    let backend = BackendImpl::new(
        Box::new(CamlSpy::new(camlspy_config, backend_config)),
        Some(backend_config),
    );
    agent_builder = agent_builder.backend(backend);
    let agent = agent_builder.build().unwrap();
    let agent_running = agent.start().unwrap();
    let ecode = child.wait().expect("failed to wait on child");
    match ecode.success() {
        true => log::info!(target: LOG_TAG, "Process exited successfully"),
        false => log::error!(target: LOG_TAG, "Process exited with failure"),
    }

    // sleep for 10 seconds to allow the agent to flush data
    thread::sleep(Duration::from_secs(10));
    agent_running.stop().unwrap();
}
