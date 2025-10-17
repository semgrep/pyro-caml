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

type event = Event.t

let src = Logs.Src.create "pyro_caml" ~doc:"Pyro Caml"

module Log = (val Logs.src_log src)

let max_frames = max_int

type child_state =
  { thread_table: (int, Stack_trace.t Stack.t) Hashtbl.t
  ; event_buffer: event_buffer }

let emit_point_event raw_backtrace =
  let raw_stack_trace =
    Stack_trace.raw_stack_trace_of_backtrace raw_backtrace
  in
  let event = Point (Mtime_clock.now_ns (), raw_stack_trace) in
  emit_event event
[@@inline always]

let enter raw_backtrace =
  let raw_stack_trace =
    Stack_trace.raw_stack_trace_of_backtrace raw_backtrace
  in
  emit_event (Enter (Mtime_clock.now_ns (), raw_stack_trace))
[@@inline always]

let exit_ () =
  let tid = (Domain.self () :> int) in
  emit_event (Exit tid)
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

let with_memprof_sampler f =
  let memprof =
    Gc.Memprof.start ~callstack_size:max_frames ~sampling_rate:1e-4 tracker
  in
  Fun.protect
    ~finally:(fun () -> Gc.Memprof.stop () ; Gc.Memprof.discard memprof)
    f

let samples_of_child_state (child_state : child_state) =
  Hashtbl.fold
    (fun _tid stack acc ->
      let sts = Stack.top_opt stack |> Option.to_list in
      sts @ acc )
    child_state.thread_table []

let create_cursor path pid = Runtime_events.create_cursor (Some (path, pid))

let empty_callbacks = Runtime_events.Callbacks.create ()

let state =
  ( Mutex.create ()
  , {thread_table= Hashtbl.create 16; event_buffer= Hashtbl.create 16} )

let with_state f =
  let m, s = state in
  Mutex.lock m ;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) (fun () -> f s)

let process_event state sample_points = function
  | Exit tid -> (
      (* pop st from thread stack *)
      let stack =
        Hashtbl.find_opt state.thread_table tid
        |> Option.value ~default:(Stack.create ())
      in
      match Stack.pop_opt stack with
      | None ->
          Log.warn (fun m ->
              m "received Exit event but no Entry on stack for thread: %d" tid ) ;
          ()
      | Some _ ->
          Hashtbl.replace state.thread_table tid stack )
  | Enter (time, e) ->
      let st = Stack_trace.t_of_raw_stack_trace e in
      let tid = st.thread_id in
      (* push new st on thread stack *)
      let stack =
        Hashtbl.find_opt state.thread_table tid
        |> Option.value ~default:(Stack.create ())
      in
      Stack.push st stack ;
      Hashtbl.replace state.thread_table tid stack ;
      sample_points := (time, st) :: !sample_points
  | Point (time, raw_st) ->
      let st = Stack_trace.t_of_raw_stack_trace raw_st in
      sample_points := (time, st) :: !sample_points
  | Partial _ ->
      ()

let read_poll ?(max_events = None) ?(callbacks = empty_callbacks) cursor
    interval =
  let interval_ns = Int64.of_float (interval *. 1e6) in
  let now = Mtime_clock.now_ns () in
  let sample_points = ref [] in
  let callbacks =
    Runtime_events.Callbacks.add_user_event perf_event_type
      (fun (_ring_buffer_index : int) (_ts : Runtime_events.Timestamp.t)
           _event_t (e : marshaled) ->
        with_state (fun state ->
            e
            |> event_of_perf_event state.event_buffer
            |> process_event state sample_points ) )
      callbacks
  in
  let _n_events = Runtime_events.read_poll cursor callbacks max_events in
  let sample_points =
    !sample_points
    |> List.filter_map (fun (time, st) ->
           if Int64.sub now time < interval_ns then Some st else None )
    |> List.sort_uniq (fun a b ->
           Int.compare a.Stack_trace.thread_id b.Stack_trace.thread_id )
  in
  let point_thids =
    List.map (fun st -> st.Stack_trace.thread_id) sample_points
  in
  let state_samples =
    with_state (fun state -> samples_of_child_state state)
    |> List.filter (fun st ->
           not (List.mem st.Stack_trace.thread_id point_thids) )
  in
  sample_points @ state_samples
