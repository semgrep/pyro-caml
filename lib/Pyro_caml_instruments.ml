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

(* Each sampled allocation gets a fresh id that is (a) sent with its Alloc
   point and (b) returned as the block's memprof tracked value, so the matching
   Dealloc can reference the block by id alone instead of re-sending its
   callstack. *)
let fresh_id () = Oo.id (object end)

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
   memprof backtraces are richer anyway). In alloc, we create a new id before
   emitting it and persisting the id to be used in promotion emitting a
   deallocation event *)
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

type diagnostics = {
  total_lost_events : int;
  orphan_part_drops : int;
  overflow_part_drops : int;
}

type sample_point = {
  time: float;
  stack_trace: Stack_trace.t;
  n_samples: int;
  size: int;
  kind: point_kind;
  id: int;
}

type gc_sample = {
  stack_trace: Stack_trace.t;
  duration_ns: int;
}

type read_poll_output = {
    now : float;
    sample_points: sample_point list;
    diagnostics: diagnostics;
    gc_samples: gc_sample list;
}

(* Minimize work we do in process event since the instrumented program can write
   events quickly and so we need to keep pace while polling if we can *)
let add_point raw_points = function
  | Some raw_point -> raw_points := raw_point :: !raw_points
  | None -> ()

(*****************************************************************************)
(* GC phase spans (gc_time flamegraph) *)
(*****************************************************************************)
(* The OCaml runtime already writes nested runtime_begin/runtime_end span events
   per domain (identified by ring buffer index) for every GC phase into the same
   ring we drain for perf events. We turn these into a flamegraph of where GC
   time goes via self-time attribution: the wall-clock interval between two
   consecutive events on a ring is credited to that ring's currently-active
   phase path (root..leaf). A span can begin in one read_poll and end in the
   next, so the per-ring stack persists across calls in [gc_rings]. *)
type gc_ring_state = {
  mutable stack : Runtime_events.runtime_phase list;
  mutable last_ts : int64;
  mutable has_last : bool;
}

let gc_rings : (int, gc_ring_state) Hashtbl.t = Hashtbl.create 8

let gc_ring_state ring =
  match Hashtbl.find_opt gc_rings ring with
  | Some s -> s
  | None ->
      let s = { stack = []; last_ts = 0L; has_last = false } in
      Hashtbl.add gc_rings ring s;
      s

(* EV_DOMAIN_CONDITION_WAIT spans idle time in Condition.wait, not GC work, and
   can be arbitrarily long; excluding it keeps the gc_time profile honest.
   Everything else is GC-related and is kept. *)
let gc_phase_tracked (phase : Runtime_events.runtime_phase) =
  match phase with EV_DOMAIN_CONDITION_WAIT -> false | _ -> true

(* Credit the interval [last_ts, ts] on [ring] to its currently-active phase
   path, keyed by (ring, phase-name path leaf-first), then advance last_ts. *)
let gc_credit gc_durations state ring ts =
  (if state.has_last && state.stack <> [] then
     let dt = Int64.sub ts state.last_ts in
     if Int64.compare dt 0L > 0 then
       let names = List.map Runtime_events.runtime_phase_name state.stack in
       let key = (ring, names) in
       let prev =
         match Hashtbl.find_opt gc_durations key with Some v -> v | None -> 0L
       in
       Hashtbl.replace gc_durations key (Int64.add prev dt));
  state.last_ts <- ts;
  state.has_last <- true

let gc_on_begin gc_durations ring ts phase =
  let state = gc_ring_state ring in
  gc_credit gc_durations state ring (Runtime_events.Timestamp.to_int64 ts);
  if gc_phase_tracked phase then state.stack <- phase :: state.stack

let gc_on_end gc_durations ring ts phase =
  let state = gc_ring_state ring in
  gc_credit gc_durations state ring (Runtime_events.Timestamp.to_int64 ts);
  if gc_phase_tracked phase then
    match state.stack with
    | top :: rest when top = phase -> state.stack <- rest
    | _ -> ()

let gc_frame name : Stack_trace.frame =
  { name = Some name; filename = None; line = None; inlined = false }

(* One gc_sample per distinct (ring, phase path) accumulated this poll: the phase
   path becomes the flamegraph stack (leaf-first) and duration_ns is self-time
   nanoseconds, tagged with the domain (ring buffer index) as its thread. *)
let gc_samples_of_durations gc_durations =
  Hashtbl.fold
    (fun (ring, names) ns acc ->
      let stack_trace : Stack_trace.t =
        {
          frames = List.map gc_frame names;
          thread_id = ring;
          thread_name = "domain " ^ string_of_int ring;
        }
      in
      { stack_trace; duration_ns = Int64.to_int ns } :: acc)
    gc_durations []

let read_poll ?(max_events = None) cursor =
  (* Recreating the point_buffer at each read_poll causes us to orphan some
     points when a marshalled point is split across a read_poll boundary (i.e. 
     part of it in one read_poll and the remaining in the next read_poll). 
     Based on local testing, it doesn't seem like we lose too many points, and
     it is unlikely that we miss a dealloc from this because deallocs don't
     contain the callstack and so should fit within a single point. So this
     shouldn't be too much of a concern. *)
  let point_buffer = Hashtbl.create 1000 in
  (* GC phase self-times accumulated during this poll, keyed by
     (ring, phase-name path). The open-span stacks in [gc_rings] persist across
     polls; only these per-poll totals are fresh each call. *)
  let gc_durations = Hashtbl.create 32 in
  let now = Unix.gettimeofday() in
  let raw_points = ref [] in
  let callbacks =
    Runtime_events.Callbacks.create
      ~runtime_begin:(gc_on_begin gc_durations)
      ~runtime_end:(gc_on_end gc_durations)
      ~lost_events:(fun (ring_buffer_index : int) (num_lost : int) ->
        (* A ring overflowed: count the loss and drop that ring's partial
           reassembly buffer, since the parts collected so far can no longer be
           trusted. Its GC span stack is likewise untrustworthy once events were
           dropped, so reset it. *)
        ignore (Atomic.fetch_and_add total_lost_events num_lost : int);
        Hashtbl.remove point_buffer ring_buffer_index;
        Hashtbl.remove gc_rings ring_buffer_index)
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
  let diagnostics = {
    total_lost_events = Atomic.get total_lost_events;
    orphan_part_drops = Atomic.get Event.orphan_part_drops;
    overflow_part_drops = Atomic.get Event.overflow_part_drops;
  } in
  let sample_points = List.rev_map
    (fun ({ time; raw_stack_trace; n_samples; size; kind; id } : point) -> {
        time;
        stack_trace = Stack_trace.t_of_raw_stack_trace raw_stack_trace;
        n_samples;
        size;
        kind;
        id;
      })
    !raw_points
  in
  let gc_samples = gc_samples_of_durations gc_durations in
  {
    now;
    diagnostics;
    sample_points;
    gc_samples;
  }
