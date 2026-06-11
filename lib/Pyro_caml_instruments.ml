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

(*****************************************************************************)
(* Instrument side code *)
(*****************************************************************************)
(* check if OCAML_RUNTIME_EVENTS_START is set *)
(* TODO check for more specific env var? *)
let is_enabled = Sys.getenv_opt "OCAML_RUNTIME_EVENTS_START" |> Option.is_some

let emit_point_event raw_backtrace =
  let raw_stack_trace =
    Stack_trace.raw_stack_trace_of_backtrace raw_backtrace
  in
  (* Record the time via time of day so we can filter points by interval *)
  (* TODO: use monotomic time + a faster call to get the time. I tried mtime but
     that doesn't play well when linked into a rust program. Monotomic time
     would be nice so if the user/system changes the time of day we aren't
     screwed up, but for now we can assume that probably won't happen much*)
  let point = (Unix.gettimeofday (), raw_stack_trace) in
  emit_point point
[@@inline always]

let tracker : (unit, unit) Gc.Memprof.tracker =
  (* the only time we get the callstack is in alloc_minor + alloc_major. All of
     these functions are called on their own stack so we can't use
     Printexc.get_callstack in the other functions. Plus for some reason the
     memprof backtraces seem way more comprehensive than those from
     Printexc.get_callstack *)
  let alloc_minor { Gc.Memprof.callstack; _ } =
    emit_point_event callstack;
    (* Don't care about tacking on any data to memory *)
    None
  in
  let alloc_major { Gc.Memprof.callstack; _ } =
    emit_point_event callstack;
    None
  in
  let promote () = None in
  let dealloc_minor = Fun.id in
  let dealloc_major = Fun.id in
  { Gc.Memprof.alloc_minor; alloc_major; promote; dealloc_minor; dealloc_major }

(* 1e-6 is nice but chosen somewhat randomly. Too high and you end up sending
   too many points and overwhelming the profiler, too little and you don't get
   enough info *)
let with_memprof_sampler ?(sampling_rate = 1e-6) f =
  let memprof = Gc.Memprof.start ~sampling_rate tracker in
  Fun.protect
    ~finally:(fun () ->
      Gc.Memprof.stop ();
      Gc.Memprof.discard memprof)
    f

let maybe_with_memprof_sampler ?sampling_rate f =
  if is_enabled then with_memprof_sampler ?sampling_rate f else f ()

(*****************************************************************************)
(* Profiler code *)
(*****************************************************************************)
let create_cursor path pid = Runtime_events.create_cursor (Some (path, pid))

type sample_point = {
  time: float;
  stack_trace: Stack_trace.t
}

(* Minimize work we do in process event since the instrumented program can write
   events quickly and so we need to keep pace while polling if we can *)
let add_point raw_points = function
  | Some raw_point -> raw_points := raw_point :: !raw_points
  | None -> ()

let read_poll ?(max_events = None) cursor =
  let point_buffer = Hashtbl.create 1000 in
  let raw_points = ref [] in
  let callbacks =
    Runtime_events.Callbacks.create
      ~lost_events:(fun (ring_buffer_index : int) (_num_lost : int) ->
        (* If we've lost events clear that ring buffer's event buffer *)
        Hashtbl.remove point_buffer ring_buffer_index)
      ()
  in
  let callbacks =
    Runtime_events.Callbacks.add_user_event perf_event_type
      (fun (ring_buffer_index : int) (_ts : Runtime_events.Timestamp.t) _event_t
           (e : marshaled) ->
        e
        |> process_perf_event ring_buffer_index point_buffer
        |> add_point raw_points)
      callbacks
  in
  (* TODO? Multithread this? *)
  let _n_events = Runtime_events.read_poll cursor callbacks max_events in
  List.rev_map 
    (fun (time, raw_st) -> { time; stack_trace = Stack_trace.t_of_raw_stack_trace raw_st})
    !raw_points
