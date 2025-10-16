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
  My_module.do_short_thing () ;
  My_module.alloc_thing ()

let example_func () =
  (* do some thing *)
  example_func2 ()

let example_func3 () = My_module.do_long_thing ()

let () =
  Runtime_events.start () ;
  Pyro_caml.with_memprof_sampler
  @@ fun () ->
  Printf.printf "Starting loop\n" ;
  flush_all () ;
  let[@pyro_profile] do_main_thing () =
    let i = 0 in
    while i < 1 do
      example_func () ; example_func3 () ; example_func () ; example_func3 ()
    done
  in
  let d1 = Domain.spawn (fun () -> do_main_thing ()) in
  let d2 = Domain.spawn (fun () -> do_main_thing ()) in
  do_main_thing () ; Domain.join d1 ; Domain.join d2
