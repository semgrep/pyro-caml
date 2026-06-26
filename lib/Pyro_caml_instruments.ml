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

(* Process-global, monotonically increasing block ids. Each sampled allocation
   gets a fresh id that is (a) sent with its Alloc point and (b) returned as the
   block's memprof tracked value, so the matching Dealloc can reference the
   block by id alone instead of re-sending its (possibly large, multipart)
   callstack. Shared across domains, so it must be atomic. *)
let next_block_id = Atomic.make 0

let fresh_id () = Atomic.fetch_and_add next_block_id 1

(* Build and emit the Alloc point for block [id]; returns the point. *)
let emit_alloc id raw_backtrace ~n_samples ~size : point =
  let raw_stack_trace =
    Stack_trace.raw_stack_trace_of_backtrace raw_backtrace
  in
  (* Record the time via time of day so we can filter points by interval *)
  (* TODO: use monotomic time + a faster call to get the time. I tried mtime but
     that doesn't play well when linked into a rust program. Monotomic time
     would be nice so if the user/system changes the time of day we aren't
     screwed up, but for now we can assume that probably won't happen much*)
  let point = {
    time=Unix.gettimeofday ();
    raw_stack_trace;
    n_samples;
    size;
    kind = Alloc;
    id;
  } in
  emit_point point
[@@inline always]

(* Manually emit an Alloc sample point (used by the Pyro Caml PPX) for code that
   allocates little but should still produce samples. *)
let emit_point_event raw_backtrace ~n_samples ~size : point =
  emit_alloc (fresh_id ()) raw_backtrace ~n_samples ~size
[@@inline always]

(* A Dealloc carries only its block id and an empty stack trace. The rust backend
   saves the stack trace at the point of allocation so that we don't have to send
   it again and risk overwhelming the event buffer. *)
let emit_dealloc_event id : unit =
  let raw_stack_trace : Stack_trace.raw_stack_trace =
    {
      slots = [];
      domain_id = (Domain.self () :> int);
      thread_name = "";
      truncated = false;
    }
  in
  ignore
    (emit_point
       { time = Unix.gettimeofday (); raw_stack_trace; n_samples = 0; size = 0; kind = Dealloc; id })
[@@inline always]

(* We only get a callstack in alloc_minor/alloc_major: the other callbacks run
   on their own stack so Printexc.get_callstack is unavailable there (and the
   memprof backtraces are richer anyway). To support the inuse_* profiles the
   profiler needs to match a freed block back to its allocation, so each alloc
   callback assigns the block a fresh id, emits it with the Alloc point, and
   stashes just the id as that block's memprof tracked value. promote forwards
   the id across the minor->major boundary unchanged, and the dealloc callbacks
   emit a stackless Dealloc carrying that id so the profiler can net it against
   the original allocation by id alone. *)
let tracker : (int, int) Gc.Memprof.tracker =
  let alloc { Gc.Memprof.callstack; n_samples; size; _ } =
    let id = fresh_id () in
    ignore (emit_alloc id callstack ~n_samples ~size);
    Some id
  in
  let promote id = Some id in
  let dealloc id = emit_dealloc_event id in
  {
    Gc.Memprof.alloc_minor = alloc;
    alloc_major = alloc;
    promote;
    dealloc_minor = dealloc;
    dealloc_major = dealloc;
  }

let resolve_sampling_rate () =
  let failure_msg =
    "OCAML_MEMPROF_SAMPLING_RATE should have been set by the rust profiler" in
  match Sys.getenv_opt "OCAML_MEMPROF_SAMPLING_RATE" with
  | Some s -> (
      match float_of_string_opt s with
      | Some rate -> rate
      | None -> failwith failure_msg)
  | None -> failwith failure_msg

  
let with_memprof_sampler f =
  let sampling_rate = resolve_sampling_rate () in
  let memprof = Gc.Memprof.start ~sampling_rate tracker in
  Fun.protect
    ~finally:(fun () ->
      Gc.Memprof.stop ();
      Gc.Memprof.discard memprof)
    f

let maybe_with_memprof_sampler f =
  if is_enabled then with_memprof_sampler f else f ()

(*****************************************************************************)
(* Profiler code *)
(*****************************************************************************)
let create_cursor path pid = Runtime_events.create_cursor (Some (path, pid))

let total_lost_events = Atomic.make 0

type sample_point = {
  time: float;
  stack_trace: Stack_trace.t;
  n_samples: int;
  size: int;
  kind: point_kind;
  id: int;
}

type read_poll_output = {
    now : float;
    sample_points: sample_point list;
}

(* Minimize work we do in process event since the instrumented program can write
   events quickly and so we need to keep pace while polling if we can *)
let add_point raw_points = function
  | Some raw_point -> raw_points := raw_point :: !raw_points
  | None -> ()

let read_poll ?(max_events = None) cursor =
  let point_buffer = Hashtbl.create 1000 in
  let now = Unix.gettimeofday() in
  let raw_points = ref [] in
  let callbacks =
    Runtime_events.Callbacks.create
      ~lost_events:(fun (ring_buffer_index : int) (num_lost : int) ->
        (* If we've lost events clear that ring buffer's event buffer *)
        let total = Atomic.fetch_and_add total_lost_events num_lost + num_lost in
        Printf.eprintf
          "[pyro-caml] WARNING: lost %d runtime events on ring %d (total lost: %d) \n"
          num_lost ring_buffer_index total;
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
  {
    now;
    sample_points = List.rev_map
    (fun ({ time; raw_stack_trace; n_samples; size; kind; id } : point) -> {
        time;
        stack_trace = Stack_trace.t_of_raw_stack_trace raw_stack_trace;
        n_samples;
        size;
        kind;
        id;
      })
    !raw_points
  }
