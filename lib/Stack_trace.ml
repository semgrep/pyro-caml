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

type raw_stack_trace = {slots: slot array; domain_id: int; thread_name: string}

let raw_stack_trace_of_backtrace bt : raw_stack_trace =
  let did = (Domain.self () :> int) in
  let name = if Domain.is_main_domain () then "main" else string_of_int did in
  let slots = Option.value ~default:[||] Printexc.(backtrace_slots bt) in
  {slots; domain_id= did; thread_name= name}

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
  | None, Some name ->
      Some {name; filename= "<unknown>"; line= 0; inlined}
  | Some loc, None ->
      Some
        { name= "<unknown>"
        ; filename= loc.filename
        ; line= loc.line_number
        ; inlined }
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
  let frames = stack_frames_of_slots (Array.to_list raw_stack_trace.slots) in
  { frames
  ; thread_id= raw_stack_trace.domain_id
  ; thread_name= raw_stack_trace.thread_name }
