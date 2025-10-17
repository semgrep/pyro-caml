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

let src = Logs.Src.create "pyro_caml" ~doc:"Pyro Caml"

module Log = (val Logs.src_log src)

let max_frames = max_int

type event =
  | Point of (int64 * Stack_trace.raw_stack_trace)
  | Enter of Stack_trace.raw_stack_trace
  | Exit of int (* thread id *)
  | Empty

let truncate_event = function
  | Point (ts, raw_stack_trace) ->
      Option.map
        (fun rst -> Point (ts, rst))
        Stack_trace.(truncate_top_raw_stack_trace raw_stack_trace)
  | Enter raw_stack_trace ->
      Option.map
        (fun rst -> Enter rst)
        Stack_trace.(truncate_top_raw_stack_trace raw_stack_trace)
  | Exit _ as e ->
      Some e
  | Empty ->
      Some Empty

let marshal_event e =
  let marshaled_event = Marshal.to_bytes e [] in
  let len = Bytes.length marshaled_event in
  (marshaled_event, len)

let rec marshal_event_sized size e =
  let marshaled_event, len = marshal_event e in
  if len > size then Option.bind (truncate_event e) (marshal_event_sized size)
  else Some (marshaled_event, len)

let marshaled_empty = marshal_event Empty

type Runtime_events.User.tag += Perf_event_tag

let perf_event_type =
  let encode (bytes : bytes) (e : event) : int =
    let marshaled, len =
      match marshal_event_sized 1024 e with
      | Some x ->
          x
      | None ->
          Log.err (fun m -> m "Could not marshal event, sending empty") ;
          marshaled_empty
    in
    Bytes.blit marshaled 0 bytes 0 len ;
    len
  in
  let decode (bytes : bytes) (_len : int) : event =
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

let sample_rate_ns = Int64.of_float (sample_rate_ms *. 1_000_000.0)

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

let emit_point_event raw_backtrace =
  let raw_stack_trace =
    Stack_trace.raw_stack_trace_of_backtrace raw_backtrace
  in
  let event = Point (Mtime_clock.now_ns (), raw_stack_trace) in
  Runtime_events.User.write perf_event event
[@@inline always]

let record_bt raw_backtrace =
  let now = Mtime_clock.elapsed_ns () in
  if need_to_emit now then (
    Domain.DLS.set last_emitted_timestamp_key (Some now) ;
    emit_point_event raw_backtrace )
[@@inline always]

let enter raw_backtrace =
  let raw_stack_trace =
    Stack_trace.raw_stack_trace_of_backtrace raw_backtrace
  in
  Runtime_events.User.write perf_event (Enter raw_stack_trace)
[@@inline always]

let exit_ () =
  let tid = (Domain.self () :> int) in
  Runtime_events.User.write perf_event (Exit tid)
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

type child_state = {thread_table: (int, Stack_trace.t Stack.t) Hashtbl.t}

let samples_of_child_state (child_state : child_state) =
  Hashtbl.fold
    (fun _tid stack acc ->
      let sts = Stack.top_opt stack |> Option.to_list in
      sts @ acc )
    child_state.thread_table []

let create_cursor path pid = Runtime_events.create_cursor (Some (path, pid))

let empty_callbacks = Runtime_events.Callbacks.create ()

let state = (Mutex.create (), {thread_table= Hashtbl.create 16})

let with_state f =
  let m, s = state in
  Mutex.lock m ;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) (fun () -> f s)

let read_poll ?(max_events = None) ?(callbacks = empty_callbacks) cursor =
  let points = ref [] in
  let callbacks =
    Runtime_events.Callbacks.add_user_event perf_event_type
      (fun (_ring_buffer_index : int) (_ts : Runtime_events.Timestamp.t)
           _event_t (e : event) ->
        match e with
        | Exit tid ->
            (* pop st from thread stack *)
            with_state (fun state ->
                let stack =
                  Hashtbl.find_opt state.thread_table tid
                  |> Option.value ~default:(Stack.create ())
                in
                match Stack.pop_opt stack with
                | None ->
                    Log.warn (fun m ->
                        m
                          "received Exit event but no Entry on stack for \
                           thread: %d"
                          tid ) ;
                    ()
                | Some _ ->
                    Hashtbl.replace state.thread_table tid stack )
        | Enter e ->
            let st = Stack_trace.t_of_raw_stack_trace e in
            let tid = st.thread_id in
            (* push new st on thread stack *)
            with_state (fun state ->
                let stack =
                  Hashtbl.find_opt state.thread_table tid
                  |> Option.value ~default:(Stack.create ())
                in
                Stack.push st stack ;
                Hashtbl.replace state.thread_table tid stack )
        | Point (time, raw_st) ->
            let now = Mtime_clock.now_ns () in
            (* if it was within the last sampling rate include it *)
            if Int64.sub now time < sample_rate_ns then
              let st = Stack_trace.t_of_raw_stack_trace raw_st in
              points := st :: !points
        | Empty ->
            () )
      callbacks
  in
  let _n_events = Runtime_events.read_poll cursor callbacks max_events in
  let points = !points in
  let point_thids = List.map (fun st -> st.Stack_trace.thread_id) points in
  let state_samples =
    with_state (fun state -> samples_of_child_state state)
    |> List.filter (fun st ->
           not (List.mem st.Stack_trace.thread_id point_thids) )
  in
  points @ state_samples
