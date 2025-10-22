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

open Event

let src = Logs.Src.create "pyro_caml" ~doc:"Pyro Caml"

module Log = (val Logs.src_log src)

(* check if OCAML_RUNTIME_EVENTS_START is set *)
(* TODO check for more specifici env var? *)
let is_enabled = Sys.getenv_opt "OCAML_RUNTIME_EVENTS_START" |> Option.is_some

let emit_point_event raw_backtrace =
  let raw_stack_trace =
    Stack_trace.raw_stack_trace_of_backtrace raw_backtrace
  in
  let event = Point (Mtime_clock.now_ns (), raw_stack_trace) in
  emit_event event
[@@inline always]

let tracker : (unit, unit) Gc.Memprof.tracker =
  let alloc_minor {Gc.Memprof.callstack; _} =
    emit_point_event callstack ; None
  in
  let alloc_major {Gc.Memprof.callstack; _} =
    emit_point_event callstack ; None
  in
  let promote () = None in
  let dealloc_minor = Fun.id in
  let dealloc_major = Fun.id in
  {Gc.Memprof.alloc_minor; alloc_major; promote; dealloc_minor; dealloc_major}

let with_memprof_sampler ?(sampling_rate = 1e-6) f =
  let memprof = Gc.Memprof.start ~sampling_rate tracker in
  Fun.protect
    ~finally:(fun () -> Gc.Memprof.stop () ; Gc.Memprof.discard memprof)
    f

let maybe_with_memprof_sampler ?sampling_rate f =
  if is_enabled then with_memprof_sampler ?sampling_rate f else f ()

let create_cursor path pid = Runtime_events.create_cursor (Some (path, pid))

let empty_callbacks = Runtime_events.Callbacks.create ()

let process_event now interval_ns sample_points = function
  | Point (time, raw_st) ->
      if Int64.sub now time < interval_ns then
        let st = Stack_trace.t_of_raw_stack_trace raw_st in
        sample_points := st :: !sample_points
  | Partial _ ->
      ()

let read_poll ?(max_events = None) ?(callbacks = empty_callbacks) cursor
    interval =
  let event_buffer = Hashtbl.create 1000 in
  let interval_ns = Int64.of_float (interval *. 1e6) in
  let now = Mtime_clock.now_ns () in
  let sample_points = ref [] in
  let callbacks =
    Runtime_events.Callbacks.add_user_event perf_event_type
      (fun (_ring_buffer_index : int) (_ts : Runtime_events.Timestamp.t)
           _event_t (e : marshaled) ->
        e
        |> event_of_perf_event event_buffer
        |> process_event now interval_ns sample_points )
      callbacks
  in
  let _n_events = Runtime_events.read_poll cursor callbacks max_events in
  let sample_points =
    List.sort_uniq
      (fun a b -> Int.compare a.Stack_trace.thread_id b.Stack_trace.thread_id)
      !sample_points
  in
  sample_points
