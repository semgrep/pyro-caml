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

let () =
  Pyro_caml_instruments.with_memprof_sampler @@ fun () ->
  Printf.printf "Starting loop\n";
  flush_all ();
  let do_main_thing () =
    while true do
      example_func ();
      example_func3 ();
      example_func ();
      example_func3 ()
    done
  in
  let domains =
    List.init 8 (fun _ -> Domain.spawn (fun () -> do_main_thing ()))
  in
  List.iter Domain.join domains
