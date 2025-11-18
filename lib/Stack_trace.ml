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

(* What's sent via runtime events. this HAS to be marshalable*)
type raw_stack_trace = {
  slots : slot array;
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
  let slots = Option.value ~default:[||] Printexc.(backtrace_slots bt) in
  { slots; domain_id = did; thread_name = name }

(*****************************************************************************)
(* Stack frames *)
(*****************************************************************************)
(* Essentially what info we want to send via the sdk *)
(* Inlined functions are filtered out in ocaml_intf always right now, but we
   probably want to give that as an option at some point *)
(* coupling: ocaml_intf *)
type frame = {
  name : string;
  filename : string;
  line : int;
  inlined : bool; [@eq.skip]
      (* We really don't care about if a function is inlined for equality*)
}
[@@deriving eq]

let stack_frame_of_slot (slot : Printexc.backtrace_slot) : frame option =
  let loc = Printexc.Slot.location slot in
  let name = Printexc.Slot.name slot in
  let inlined = Printexc.Slot.is_inline slot in
  match (loc, name) with
  | Some loc, Some name ->
      Some { name; filename = loc.filename; line = loc.line_number; inlined }
  | None, Some name -> Some { name; filename = "<unknown>"; line = 0; inlined }
  | Some loc, None ->
      Some
        {
          name = "<unknown>";
          filename = loc.filename;
          line = loc.line_number;
          inlined;
        }
  | None, None -> None

(* Looking ahead by up to 3 can be useful for recursive functions to make them
   much more legible. E.g. List.map can be very recursive, and pyroscope has a
   ~1000 stack frame limit, so if we iterate through say, 10k items there's a
   good chance we may max out, which will cause any frames past the limit to be
   dropped and instead replaced with a single frame that says "other"*)
let compress frames =
  let rec aux acc = function
    | [] -> List.rev acc
    | f1 :: f2 :: f3 :: (f4 :: f5 :: f6 :: _ as rest)
      when equal_frame f1 f4 && equal_frame f2 f5 && equal_frame f3 f6 ->
        aux acc rest
    | f1 :: f2 :: (f3 :: f4 :: _ as rest)
      when equal_frame f1 f3 && equal_frame f2 f4 ->
        aux acc rest
    | f1 :: (f2 :: _ as rest) when equal_frame f1 f2 -> aux acc rest
    | f :: rest -> aux (f :: acc) rest
  in
  aux [] frames

let stack_frames_of_slots slots =
  slots |> List.filter_map stack_frame_of_slot |> compress

(*****************************************************************************)
(* Stack traces *)
(*****************************************************************************)
(* coupling: ocaml_intf *)
type t = { frames : frame list; thread_id : int; thread_name : string }

let t_of_raw_stack_trace raw_stack_trace =
  let frames = stack_frames_of_slots (Array.to_list raw_stack_trace.slots) in
  {
    frames;
    thread_id = raw_stack_trace.domain_id;
    thread_name = raw_stack_trace.thread_name;
  }
