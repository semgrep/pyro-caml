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

let f x y z =
  let a = x + y in
  let b = a * z in
  let c = b - y in
  c
(* [@@pyro_profile] *)

let alloc_thing () =
  let random_list =
    List.init 1000 (fun _ ->
        {x= Random.int 100000; y= string_of_int (Random.int 100000)} )
  in
  let _sorted = List.sort compare random_list in
  List.iter
    (fun x ->
      if f x.x (String.length x.y) 100 mod 10 = 10000000000 then assert false )
    random_list

class stack_of_ints =
  object (_self)
    val mutable the_list = ([] : int list) (* instance variable *)

    method push x =
      (* push method *)
      the_list <- x :: the_list

    method pop =
      (* pop method *)
      let result = List.hd the_list in
      the_list <- List.tl the_list ;
      result

    method peek =
      (* peek method *)
      List.hd the_list

    method size =
      (* size method *)
      List.length the_list
  end

let do_thing () =
  let stack = new stack_of_ints in
  for _i = 1 to 1000 do
    stack#push (Random.int 100000)
  done ;
  while stack#size > 0 do
    let _ = stack#pop in
    ()
  done ;
  alloc_thing ()

let do_short_thing () = alloc_thing ()

let sleep time = Unix.sleepf time [@@pyro_profile]

let do_long_thing () =
  alloc_thing () ;
  alloc_thing () ;
  alloc_thing () ;
  comp_and_callback Fun.id ;
  sleep 0.01

(* Example object *)
