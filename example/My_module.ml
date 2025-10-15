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

type t = {x: int; y: string}

external comp_and_callback : (unit -> unit) -> unit = "ml_comp_and_callback"

let alloc_thing () =
  Perf_event.enter Printexc.(get_callstack max_int) ;
  let random_list =
    List.init 1000 (fun _ ->
        {x= Random.int 100000; y= string_of_int (Random.int 100000)} )
  in
  let _sorted = List.sort compare random_list in
  List.iter
    (fun x -> if x.x mod 10000 = 1111111111 then assert false)
    random_list ;
  Perf_event.exit_ ()

let do_thing () = alloc_thing ()

let do_short_thing () = alloc_thing ()

let sleep time =
  Perf_event.enter Printexc.(get_callstack max_int) ;
  Unix.sleepf time ;
  Perf_event.exit_ ()

let do_long_thing () =
  Perf_event.enter Printexc.(get_callstack max_int) ;
  alloc_thing () ;
  alloc_thing () ;
  alloc_thing () ;
  comp_and_callback Fun.id ;
  sleep 0.01 ;
  Perf_event.exit_ ()
