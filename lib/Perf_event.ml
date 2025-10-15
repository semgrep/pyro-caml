(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

type Runtime_events.User.tag += Perf_event_tag

let perf_event_type =
  let encode (bytes : bytes) (raw_stack_trace : Stack_trace.raw_stack_trace) :
      int =
    let marshaled = Marshal.to_bytes raw_stack_trace [] in
    let len = Bytes.length marshaled in
    Bytes.blit marshaled 0 bytes 0 len ;
    len
  in
  let decode (bytes : bytes) (_len : int) : Stack_trace.raw_stack_trace =
    Marshal.from_bytes bytes 0
  in
  Runtime_events.Type.register ~encode ~decode

let perf_event =
  Runtime_events.User.register "Perf_event" Perf_event_tag perf_event_type

(* Per second *)
let sample_rate =
  Sys.getenv_opt "PERF_EVENT_SAMPLE_RATE"
  |> Option.map int_of_string |> Option.value ~default:100

let sample_rate_ms = 1000.0 /. float_of_int sample_rate

let last_emitted_timestamp_key : int64 option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

let need_to_emit (now : int64) =
  match Domain.DLS.get last_emitted_timestamp_key with
  | None ->
      true
  | Some last_emitted_timestamp ->
      let elapsed = Int64.sub now last_emitted_timestamp in
      let elapsed_ms = Int64.div elapsed 1_000_000L |> Int64.to_float in
      elapsed_ms >= sample_rate_ms

let emit_bt ?(repeat = 0) raw_backtrace =
  let slots = Stack_trace.slots_of_raw_backtrace raw_backtrace in
  let raw_stack_trace = Stack_trace.raw_stack_trace_of_slots slots in
  for _i = 0 to repeat do
    Runtime_events.User.write perf_event raw_stack_trace
  done
[@@inline always]

let record_bt raw_backtrace =
  let now = Mtime_clock.elapsed_ns () in
  if need_to_emit now then (
    Domain.DLS.set last_emitted_timestamp_key (Some now) ;
    emit_bt raw_backtrace )
[@@inline always]

let record () =
  let raw_backtrace = Printexc.get_raw_backtrace () in
  record_bt raw_backtrace
[@@inline always]

let tracker : (unit, unit) Gc.Memprof.tracker =
  let alloc_minor {Gc.Memprof.callstack; _} = record_bt callstack ; None in
  let alloc_major {Gc.Memprof.callstack; _} = record_bt callstack ; None in
  let promote () = None in
  let dealloc_minor = Fun.id in
  let dealloc_major = Fun.id in
  {Gc.Memprof.alloc_minor; alloc_major; promote; dealloc_minor; dealloc_major}

let with_memprof_sampler f =
  let memprof =
    Gc.Memprof.start ~callstack_size:15 ~sampling_rate:1e-4 tracker
  in
  Fun.protect
    ~finally:(fun () -> Gc.Memprof.stop () ; Gc.Memprof.discard memprof)
    f

let create_cursor path pid =
  (* Check if path/pid.events exists *)
  let events_file = Printf.sprintf "%s/%d.events" path pid in
  while not (Sys.file_exists events_file) do
    Unix.sleepf 0.1
  done ;
  try Ok (Runtime_events.create_cursor (Some (path, pid)))
  with Failure msg -> Error (Printf.sprintf "Failed to create cursor: %s" msg)

let empty_callbacks = Runtime_events.Callbacks.create ()

let read_poll ?(max_events = None) ?(callbacks = empty_callbacks) cursor_opt =
  match cursor_opt with
  | Error msg ->
      Printf.eprintf "No cursor, cannot read events: %s\n%!" msg ;
      []
  | Ok cursor ->
      let sts = ref [] in
      let callbacks =
        Runtime_events.Callbacks.add_user_event perf_event_type
          (fun (_ring_buffer_index : int) (_ts : Runtime_events.Timestamp.t)
               _event_t (raw_stack_trace : Stack_trace.raw_stack_trace) ->
            let st = Stack_trace.t_of_raw_stack_trace raw_stack_trace in
            sts := st :: !sts )
          callbacks
      in
      let _n_events = Runtime_events.read_poll cursor callbacks max_events in
      List.rev !sts
