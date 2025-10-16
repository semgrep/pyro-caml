use std::path::Path;

use ocaml::Runtime;
use pyroscope::{
    backend::{BackendConfig, StackFrame, StackTrace},
    PyroscopeError,
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
    name: String,
    filename: String,
    line: ocaml::Int,
    inlined: bool,
}

// convert between CamlStackFrame and StackFrame
impl From<CamlStackFrame> for StackFrame {
    fn from(frame: CamlStackFrame) -> Self {
        StackFrame {
            module: None,
            name: Some(frame.name),
            filename: Some(frame.filename),
            relative_path: None,
            absolute_path: None,
            line: Some(frame.line as u32),
        }
    }
}

#[derive(ocaml::ToValue, ocaml::FromValue)]
struct CamlStackTrace {
    frames: ocaml::List<CamlStackFrame>,
    thread_id: ocaml::Int,
    thread_name: String,
}

impl CamlStackTrace {
    fn into_stack_trace(self, backend_config: &BackendConfig, pid: u32) -> StackTrace {
        let frames = self
            .frames
            .into_vec()
            .into_iter()
            .filter(|f| !f.inlined)
            .map(|f| f.into())
            .collect();
        StackTrace::new(
            backend_config,
            Some(pid),
            Some(self.thread_id as u64),
            Some(self.thread_name),
            frames,
        )
    }
}

pub type Cursor = ocaml::Value;

ocaml::import! {
  fn read_poll_ml(cursor:Cursor) -> ocaml::List<CamlStackTrace>;
  fn create_cursor_ml(path:String, pid:ocaml::Int) -> Cursor;
}

pub fn read_poll(
    gc: &Runtime,
    cursor: Cursor,
    backend_config: &BackendConfig,
    pid: u32,
) -> Result<Vec<StackTrace>, CamlIntfError> {
    Ok(unsafe { read_poll_ml(gc, cursor) }?
        .into_vec()
        .into_iter()
        .map(|st| st.into_stack_trace(backend_config, pid))
        .collect())
}

pub fn create_cursor(gc: &Runtime, path: &Path, pid: u32) -> Cursor {
    unsafe { create_cursor_ml(gc, path.to_str().unwrap().to_string(), pid as ocaml::Int) }
        .expect("failed to create cursor")
}
