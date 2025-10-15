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
  (* let cursor = Runtime_events.create_cursor (Some (Sys.getcwd (), 23674)) in *)
  let cursor = Runtime_events.create_cursor None in
  Perf_event.with_memprof_sampler
  @@ fun () ->
  Printf.printf "Starting loop\n" ;
  flush_all () ;
  (* while true do *)
  for _i = 1 to 10 do
    example_func () ; example_func3 () ; example_func () ; example_func3 ()
  done ;
  (* done ; *)
  let sts = Perf_event.read_poll (Ok cursor) in
  Printf.printf "Example: Wrote %d stack\n" (List.length sts)
