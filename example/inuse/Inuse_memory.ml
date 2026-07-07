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

(* Test that allocates and holds memory at varying levels to test inuse_space and
   inuse_objects.

   Level 1: 1 million records, ~30 MiB
   Level 2: 4 million records, ~122 MiB
   Level 3: 2 million records, ~61 MiB
 *)
 type t = { _x : int; _y : string }
let retained : t array ref = ref [||]
let retain_known_count n = retained := Array.init n (fun i -> { _x = i; _y = "" })
let release_retained () = retained := [||]

(* Expected live bytes / object count for [retain_known_count n], for the test
   to print so the inuse_* readings can be checked against them. *)
let retained_bytes n = (3 * n + n + 1) * (Sys.word_size / 8)
let retained_objects n = n + 1

let run_staircase () =
  (* Need to hold for more than the 15s reporting window so that we have enough
     data points *)
  let hold_seconds = 40.0 in
  let hold seconds =
    let deadline = Unix.gettimeofday () +. seconds in
    (* Continue allocating a bit of memory *)
    while Unix.gettimeofday () < deadline do
      ignore (Sys.opaque_identity (Bytes.create 4096));
      Unix.sleepf 0.02
    done
  in
  let level label n =
    retain_known_count n;
    (* Reclaim the previous level's garbage now so the drop is prompt. *)
    Gc.full_major ();
    Printf.printf
      "[staircase] %s: retaining %d records -> inuse_space ~= %d MiB, \
       inuse_objects ~= %d; holding %.0fs\n%!"
      label n
      (retained_bytes n / (1024 * 1024))
      (retained_objects n) hold_seconds;
    hold hold_seconds
  in
  level "level 1 (up)" 1_000_000;
  level "level 2 (up)" 4_000_000;
  level "level 3 (down)" 2_000_000;
  release_retained ();
  Gc.full_major ();
  Printf.printf "[staircase] released -> inuse_space ~= 0; holding %.0fs\n%!"
    hold_seconds;
  Unix.sleepf hold_seconds

let () =
  Pyro_caml_instruments.with_memprof_sampler @@ run_staircase