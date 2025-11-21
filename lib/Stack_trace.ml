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
(* Slots *)
(*****************************************************************************)

type slot = Printexc.backtrace_slot

let equal_slot s1 s2 =
  let name_eq =
    Option.equal String.equal (Printexc.Slot.name s1) (Printexc.Slot.name s2)
  in
  let loc1 = Printexc.Slot.location s1 in
  let loc2 = Printexc.Slot.location s2 in
  let loc_eq =
    match (loc1, loc2) with
    | None, None -> true
    | Some l1, Some l2 ->
        l1.filename = l2.filename && l1.line_number = l2.line_number
    | _, _ -> false
  in
  name_eq && loc_eq

(* Looking ahead by up to 3 can be useful for recursive functions to make them
   much more legible. E.g. List.map can be very recursive, and pyroscope has a
   ~1000 stack frame limit, so if we iterate through say, 10k items there's a
   good chance we may max out, which will cause any frames past the limit to be
   dropped and instead replaced with a single frame that says "other".

   It's also nice to do this on the client side since it reduces how much data
   we have to turn into runtime events and reduces the overall number of events
   since there will be less partial events needed.
*)
let compress_slot_array array =
  (* TODO how to do this inline w/an array? *)
  let array_list = Array.to_list array in
  let rec aux acc = function
    | [] -> List.rev acc
    | s1 :: s2 :: s3 :: (s4 :: s5 :: s6 :: _ as rest)
      when equal_slot s1 s4 && equal_slot s2 s5 && equal_slot s3 s6 ->
        aux acc rest
    | s1 :: s2 :: (s3 :: s4 :: _ as rest)
      when equal_slot s1 s3 && equal_slot s2 s4 ->
        aux acc rest
    | s1 :: (s2 :: _ as rest) when equal_slot s1 s2 -> aux acc rest
    | s :: rest -> aux (s :: acc) rest
  in
  aux [] array_list

(* What's sent via runtime events. this HAS to be marshalable*)
type raw_stack_trace = {
  slots : slot list;
  domain_id : int;
  thread_name : string;
}

let raw_stack_trace_of_backtrace bt : raw_stack_trace =
  (* Use the domain as the ID since runtime event sampling happens per domain *)
  (* TODO? also somehow include thread id *)
  let did = (Domain.self () :> int) in
  (* Nice to call it main but probably not necessary *)
  let name = if Domain.is_main_domain () then "main" else string_of_int did in
  (* if there aren't any slots then not much we can do *)
  let slots =
    bt
    |> Printexc.(backtrace_slots)
    |> Option.map compress_slot_array
    |> Option.value ~default:[]
  in
  { slots; domain_id = did; thread_name = name }

(*****************************************************************************)
(* Stack frames *)
(*****************************************************************************)
(* Essentially what info we want to send via the sdk *)
(* Inlined functions are filtered out in ocaml_intf always right now, but we
   probably want to give that as an option at some point *)
(* coupling: ocaml_intf *)
type frame = {
  name : string option;
  filename : string option;
  line : int option;
  inlined : bool; [@eq.skip]
      (* We really don't care about if a function is inlined for equality*)
}
[@@deriving eq]

let other_frame =
  { name = Some "other"; filename = None; line = None; inlined = false }

let stack_frame_of_slot (slot : Printexc.backtrace_slot) : frame =
  let filename, line =
    match Printexc.Slot.location slot with
    | Some loc -> (Some loc.filename, Some loc.line_number)
    | None -> (None, None)
  in
  let name = Printexc.Slot.name slot in
  let inlined = Printexc.Slot.is_inline slot in
  { name; filename; line; inlined }

let stack_frames_of_slots = List.map stack_frame_of_slot

(*****************************************************************************)
(* Stack traces *)
(*****************************************************************************)
(* coupling: ocaml_intf *)
type t = { frames : frame list; thread_id : int; thread_name : string }

let t_of_raw_stack_trace raw_stack_trace =
  let frames = stack_frames_of_slots raw_stack_trace.slots in
  {
    frames;
    thread_id = raw_stack_trace.domain_id;
    thread_name = raw_stack_trace.thread_name;
  }
