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
let example_func2 () =
  My_module.do_short_thing ();
  My_module.alloc_thing ()

let example_func () =
  (* do some thing *)
  example_func2 ()

let example_func3 () = My_module.do_long_thing ()

(* Each call allocates exactly [known_allocs_per_call] records (see
   My_module.alloc_known_count). It runs once per loop iteration below, so the
   alloc_objects (Count) profile for My_module.alloc_known_count should read
   ~[known_allocs_per_call * loop_iterations * domain_count] objects, modulo
   memprof sampling noise. With the values below that is
   1_000_000 * 100 * 8 = 800_000_000. *)
let known_allocs_per_call = 1_000_000
let example_func4 () = My_module.alloc_known_count known_allocs_per_call

(* The original churn workload: allocates furiously and retains nothing. Good
   for the alloc_*/cpu profiles and the memtrace comparison, but its live set is
   a sub-millisecond sawtooth that Pyroscope's 10s reporting window cannot
   resolve — so it is useless for *validating* inuse. *)
let run_churn () =
  Printf.printf "Starting loop\n";
  flush_all ();
  let loop_iterations = 100 in
  let domain_count = 8 in
  let do_main_thing () =
    for _ = 1 to loop_iterations do
      example_func ();
      example_func3 ();
      example_func ();
      example_func3 ();
      example_func4 ()
    done
  in
  let domains =
    List.init domain_count (fun _ -> Domain.spawn (fun () -> do_main_thing ()))
  in
  List.iter Domain.join domains

(* The inuse validation workload. Pyroscope samples the live set once per ~10s,
   so instead of a fast sawtooth we drive a *staircase* of retained memory and
   hold each level for [hold_seconds] (>= several reports) — long enough that
   multiple reports land on the same flat, known level. We force a full major
   collection after each transition so the previous level's now-dead blocks are
   reclaimed promptly (their deallocs fire and net out), making the steps crisp.

   Expected inuse_space plateaus (~32 bytes/record) and inuse_objects plateaus
   (~record count) are printed below; read them back off the time-series with a
   narrow time range / average aggregation. The down-steps are the key check:
   inuse_space must DROP when memory is freed (a gauge), whereas alloc_space on
   the same run only ever climbs (cumulative). *)
let run_staircase () =
  (* Hold each level for this many seconds. Must be >= several Pyroscope 10s
     reporting windows for clean plateaus; override with PYRO_TEST_HOLD (e.g. a
     small value for a quick smoke test). *)
  let hold_seconds =
    match Sys.getenv_opt "PYRO_TEST_HOLD" with
    | Some s -> ( match float_of_string_opt s with Some f -> f | None -> 40.0)
    | None -> 40.0
  in
  (* Hold for [seconds] while trickling tiny throwaway allocations. memprof
     delivers its dealloc callbacks at allocation safe points, NOT synchronously
     from Gc.full_major, so a *quiet* sleep after freeing a level leaves those
     frees pending — they never get emitted and inuse never drops (the freed
     level lingers, e.g. ~65 MiB stuck at the end). The trickle (~200 KB/s of
     immediately-dead garbage on its own stack, negligible vs the retained
     levels) keeps safe points coming so frees are delivered promptly. *)
  let hold seconds =
    let deadline = Unix.gettimeofday () +. seconds in
    while Unix.gettimeofday () < deadline do
      ignore (Sys.opaque_identity (Bytes.create 4096));
      Unix.sleepf 0.02
    done
  in
  let level label n =
    My_module.retain_known_count n;
    (* Reclaim the previous level's garbage now so the drop is prompt. *)
    Gc.full_major ();
    Printf.printf
      "[staircase] %s: retaining %d records -> inuse_space ~= %d MiB, \
       inuse_objects ~= %d; holding %.0fs\n%!"
      label n
      (My_module.retained_bytes n / (1024 * 1024))
      (My_module.retained_objects n) hold_seconds;
    hold hold_seconds
  in
  level "level 1 (up)" 1_000_000;
  level "level 2 (up)" 4_000_000;
  level "level 3 (down)" 2_000_000;
  My_module.release_retained ();
  Gc.full_major ();
  Printf.printf "[staircase] released -> inuse_space ~= 0; holding %.0fs\n%!"
    hold_seconds;
  Unix.sleepf hold_seconds

let () =
  Pyro_caml_instruments.with_memprof_sampler @@ fun () ->
  match Sys.getenv_opt "PYRO_TEST" with
  | Some "staircase" -> run_staircase ()
  | _ -> run_churn ()
