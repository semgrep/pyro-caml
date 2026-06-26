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

let () =
  Pyro_caml_instruments.with_memprof_sampler @@ fun () ->
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
