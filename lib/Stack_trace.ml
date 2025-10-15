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

type raw_stack_trace = {slots: slot list; thread_id: int; thread_name: string}

let map_raw_backtrace_slot f slot =
  let rec map_raw_backtrace_slot' f slot acc =
    let acc' = f slot :: acc in
    match Printexc.get_raw_backtrace_next_slot slot with
    | None ->
        acc'
    | Some slot' ->
        map_raw_backtrace_slot' f slot' acc'
  in
  map_raw_backtrace_slot' f slot []

let map_raw_backtrace f bt =
  let length = Printexc.raw_backtrace_length bt in
  let rec aux i acc =
    if i < length then
      let slot = Printexc.get_raw_backtrace_slot bt i in
      let slot = f slot in
      aux (i + 1) (slot :: acc)
    else List.rev acc
  in
  aux 0 []

let slots_of_raw_backtrace =
  map_raw_backtrace Printexc.convert_raw_backtrace_slot

let get_callstack ?(max_frames = 1000) () =
  Printexc.get_callstack max_frames |> slots_of_raw_backtrace |> List.tl
[@@inline always]

let print_callstack ?max_frames () =
  get_callstack ?max_frames ()
  |> List.mapi Printexc.Slot.format
  |> List.filter_map Fun.id |> String.concat "\n" |> print_endline

let raw_stack_trace_of_slots slots : raw_stack_trace =
  let did = (Domain.self () :> int) in
  let name = if Domain.is_main_domain () then "main" else string_of_int did in
  {slots; thread_id= did; thread_name= name}

let get_raw_stack_trace ?max_frames () =
  let cs = get_callstack ?max_frames () in
  (* drop first slot since it's always this function *)
  let cs = List.tl cs in
  raw_stack_trace_of_slots cs
[@@inline always]

(*****************************************************************************)
(* Stack frames *)
(*****************************************************************************)
(* coupling: ocaml_intf *)
type frame = {name: string; filename: string; line: int; inlined: bool}

let stack_frame_of_slot (slot : Printexc.backtrace_slot) : frame option =
  let loc = Printexc.Slot.location slot in
  let name = Printexc.Slot.name slot in
  let inlined = Printexc.Slot.is_inline slot in
  match (loc, name) with
  | Some loc, Some name ->
      Some {name; filename= loc.filename; line= loc.line_number; inlined}
  | __else__ ->
      None

let stack_frames_of_slots slots =
  slots
  |> List.map (fun slot -> slot |> stack_frame_of_slot)
  |> List.filter_map Fun.id

(*****************************************************************************)
(* Stack traces *)
(*****************************************************************************)
(* coupling: ocaml_intf *)
type t = {frames: frame list; thread_id: int; thread_name: string}

let t_of_raw_stack_trace raw_stack_trace =
  let frames = stack_frames_of_slots raw_stack_trace.slots in
  { frames
  ; thread_id= raw_stack_trace.thread_id
  ; thread_name= raw_stack_trace.thread_name }
