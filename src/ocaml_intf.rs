use std::{collections::LinkedList, path::Path};

use nix::libc::pthread_t;
use ocaml::{FromValue, Runtime};
use pyroscope::{
    PyroscopeError, ThreadId,
    backend::{BackendConfig, StackFrame, StackTrace},
};

#[derive(Debug, Clone)]
pub struct CamlIntfError(String);

impl std::fmt::Display for CamlIntfError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "OcamlIntfError: {}", self.0)
    }
}

impl From<CamlIntfError> for PyroscopeError {
    fn from(err: CamlIntfError) -> Self {
        pyroscope::error::PyroscopeError::new(&err.0)
    }
}

// Convert from ocaml::Error to OcamlIntfError
impl From<ocaml::Error> for CamlIntfError {
    fn from(err: ocaml::Error) -> Self {
        if let ocaml::Error::Caml(ocaml::CamlError::Exception(exc)) = &err {
            // # Safety
            // exc.exception_to_string() is unsafe because it may dereference
            // a raw pointer inside the ocaml runtime. However, since we have
            // an &Runtime in the call stack, we assume the caller has ensured
            // the runtime is valid. (I think)
            match unsafe { exc.exception_to_string() } {
                Ok(s) => CamlIntfError(format!("Ocaml exception: {}", s)),
                Err(utf8error) => CamlIntfError(format!(
                    "Ocaml exception (failed to convert to string: {}): {:?}",
                    utf8error, exc
                )),
            }
        } else {
            CamlIntfError(format!("Ocaml error: {:?}", err))
        }
    }
}

#[derive(ocaml::ToValue, ocaml::FromValue, Debug)]
struct CamlStackFrame {
    name: Option<String>,
    filename: Option<String>,
    line: Option<usize>,
    inlined: bool,
}

// convert between CamlStackFrame and StackFrame
impl From<CamlStackFrame> for StackFrame {
    fn from(frame: CamlStackFrame) -> Self {
        StackFrame {
            module: None,
            name: frame.name,
            filename: frame.filename,
            relative_path: None,
            absolute_path: None,
            line: frame.line.map(|l| l as u32),
        }
    }
}

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlStackTrace {
    frames: LinkedList<CamlStackFrame>,
    pub thread_id: usize,
    thread_name: String,
}

impl CamlStackTrace {
    pub fn into_stack_trace(self, backend_config: &BackendConfig, pid: u32) -> StackTrace {
        let frames = self
            .frames
            .into_iter()
            .filter(|f| !f.inlined)
            .map(|f| f.into())
            .collect();
        StackTrace::new(
            backend_config,
            Some(pid),
            Some(ThreadId::from(self.thread_id as pthread_t)),
            Some(self.thread_name),
            frames,
        )
    }
}

pub type Cursor = ocaml::Value;

ocaml::import! {
  fn read_poll_ml(cursor:Cursor) -> CamlReadPollOutput;
  fn create_cursor_ml(path:String, pid:ocaml::Int) -> Cursor;
}

/// Decodes the OCaml `diagnostics` record `{ total_lost_events : int;
/// orphan_part_drops : int; overflow_part_drops : int }`
/// from lib/Pyro_caml_instruments.ml. ocaml-rs decodes positionally, so the
/// field order must match the OCaml record.
#[derive(ocaml::ToValue, ocaml::FromValue)]
struct CamlDiagnostics {
    total_lost_events: isize,
    orphan_part_drops: isize,
    overflow_part_drops: isize,
}

/// Cumulative, monotonically-increasing event-loss counters reported by OCaml on
/// every poll. [total_lost_events] is ring-buffer overflow; the rest are
/// multipart-reassembly drops (see lib/Pyro_caml_instruments.mli).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Diagnostics {
    pub total_lost_events: u64,
    pub orphan_part_drops: u64,
    pub overflow_part_drops: u64,
}

impl From<CamlDiagnostics> for Diagnostics {
    fn from(d: CamlDiagnostics) -> Self {
        Diagnostics {
            total_lost_events: d.total_lost_events.max(0) as u64,
            orphan_part_drops: d.orphan_part_drops.max(0) as u64,
            overflow_part_drops: d.overflow_part_drops.max(0) as u64,
        }
    }
}

/// Decodes the OCaml `read_poll_output` record `{ now : float; sample_points :
/// sample_point list; diagnostics : diagnostics; gc_samples : gc_sample list }`
/// from lib/Pyro_caml_instruments.ml. As with [CamlSamplePoint], ocaml-rs decodes
/// this positionally, so the field order (now, sample_points, diagnostics,
/// gc_samples) must match the OCaml record. `now` is the reference timestamp
/// captured on the OCaml side at the start of the poll.
#[derive(ocaml::ToValue, ocaml::FromValue)]
struct CamlReadPollOutput {
    now: f64,
    sample_points: ocaml::List<ocaml::Value>,
    diagnostics: CamlDiagnostics,
    gc_samples: ocaml::List<ocaml::Value>,
}

/// A slice of GC time attributed to one runtime phase path, for the gc_time
/// flamegraph.
#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlGcSample {
    pub stack_trace: CamlStackTrace,
    pub duration_ns: isize,
}

/// Result of [read_poll]: the reference timestamp captured by OCaml together
/// with the decoded sample points, GC phase samples, and cumulative loss
/// diagnostics.
pub struct ReadPollOutput {
    pub now: f64,
    pub sample_points: Vec<CamlSamplePoint>,
    pub diagnostics: Diagnostics,
    pub gc_samples: Vec<CamlGcSample>,
}

/// A single profiling sample. NOTE: field order is part of the FFI contract.
/// ocaml-rs decodes this struct positionally from the OCaml `sample_point`
/// record `{ time : float; stack_trace : Stack_trace.t; n_samples : int;
/// size: int; kind: Event.point_kind; id: int }` in lib/Pyro_caml_instruments.ml,
/// so these fields must stay in that same order. Reordering either side without
/// the other silently mis-decodes. See ocaml-rs docs for type conversion
/// information (https://zshipko.github.io/ocaml-rs/02_type_conversion.html)
#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlSamplePoint {
    pub time: f64,
    pub stack_trace: CamlStackTrace,
    pub n_samples: isize,
    pub size: isize,
    pub kind: isize,
    pub id: isize,
}

pub fn read_poll(gc: &Runtime, cursor: Cursor) -> Result<ReadPollOutput, CamlIntfError> {
    // # Safety
    //
    // it is unclear why this function is unsafe from the ocaml-rs docs
    // but we assume the caller must ensure the gc is valid, and we convert the
    // path and int for the caller, so there is no risk they coerced a bad
    // value into an ocaml::Value
    let output = unsafe { read_poll_ml(gc, cursor) }?;
    Ok(ReadPollOutput {
        now: output.now,
        diagnostics: output.diagnostics.into(),
        sample_points: output
            .sample_points
            .into_vec()
            .into_iter()
            .map(<CamlSamplePoint>::from_value)
            .collect(),
        gc_samples: output
            .gc_samples
            .into_vec()
            .into_iter()
            .map(<CamlGcSample>::from_value)
            .collect(),
    })
}

pub fn create_cursor(gc: &Runtime, path: &Path, pid: u32) -> Cursor {
    // Safety
    //
    // it is unclear why this function is unsafe from the ocaml-rs docs but we
    // assume the caller must ensure the gc is valid, and we conver the path and
    // int for the caller, so there is no risk they coerced a bad value into an
    // ocaml::Value
    unsafe { create_cursor_ml(gc, path.to_str().unwrap().to_string(), pid as ocaml::Int) }
        .expect("failed to create cursor")
}
