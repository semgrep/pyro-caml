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

type t = { x : int; y : string }

external comp_and_callback : (unit -> unit) -> unit = "ml_comp_and_callback"

let f x y z =
  let a = x + y in
  let b = a * z in
  let c = b - y in
  c

let rec non_tail_recursive_fold_right f lst acc =
  match lst with
  | [] -> acc
  | x :: xs -> f x (non_tail_recursive_fold_right f xs acc)

let alloc_thing () =
  let random_list =
    List.init 2048 (fun _ ->
        { x = Random.int 100000; y = string_of_int (Random.int 100000) })
  in
  let _sorted = List.sort compare random_list in
  non_tail_recursive_fold_right
    (fun x acc ->
      if f x.x (String.length x.y) 100 mod 10 = 42 then assert false;
      acc)
    random_list ()

class stack_of_ints =
  object (_self)
    val mutable the_list = ([] : int list) (* instance variable *)

    method push x =
      (* push method *)
      the_list <- x :: the_list

    method pop =
      (* pop method *)
      let result = List.hd the_list in
      the_list <- List.tl the_list;
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
  done;
  while stack#size > 0 do
    let _ = stack#pop in
    ()
  done;
  alloc_thing ()

let do_short_thing () = alloc_thing ()

let do_long_thing () =
  alloc_thing ();
  alloc_thing ();
  alloc_thing ();
  comp_and_callback alloc_thing

(* Allocates exactly [n] heap blocks — one [t] record per iteration — so the
   alloc_objects (Count) estimate for this frame can be checked against a
   number we control. [x = i] makes each record distinct so flambda can't
   hoist a loop-invariant allocation out, and [Sys.opaque_identity] is an
   optimizer barrier that stops the record being scalar-replaced or
   eliminated as dead. [y] is the static empty string (an atom), so it adds
   no per-iteration allocation: the count is exactly [n] records. *)
let alloc_known_count n =
  for i = 1 to n do
    ignore (Sys.opaque_identity { x = i; y = "" })
  done

(* Example object *)
