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
(* Event *)
(*****************************************************************************)

(* runtime events only support payloads of max 1024 bytes, and any larger will
   raise an exception. Since some callstacks can be LARGE!! we break up the
   callstack into a multipart message that is then reassembled by the
   profiler. *)
type t = { bytes : Bytes.t; part : int; part_count : int }

(* The actual underlying data we're transmitting, a stack trace with a
   timestamp *)
type point = { time: float; raw_stack_trace: Stack_trace.raw_stack_trace; n_samples: int; size: int}
type marshaled = bytes * int

let split_bytes bytes size =
  let rec aux offset parts =
    if offset >= Bytes.length bytes then List.rev parts
    else
      let len = min size (Bytes.length bytes - offset) in
      let part = Bytes.sub bytes offset len in
      aux (offset + len) (part :: parts)
  in
  aux 0 []

let marshal e =
  let marshaled_obj = Marshal.to_bytes e [] in
  let len = Bytes.length marshaled_obj in
  (marshaled_obj, len)

(* 900 is chosen since when we break the event up into a partial event, we need
   to save some room for the other parts of the Partial data structure besides
   the bytes*)
(* TODO be more clever about max size *)
let marshal_point ?(max_size = 900) (p : point) =
  let marshaled_point, _len = marshal p in
  (* Max size of runtime event type payload is 1024, but we want to stay
     slightly under that so we can store metadata about which part this is *)
  (* https://ocaml.org/manual/5.3/api/Runtime_events.Type.html *)
  let parts = split_bytes marshaled_point max_size in
  let part_count = List.length parts in
  let mk_part part part_bytes = { bytes = part_bytes; part; part_count } in
  parts |> List.mapi (fun i part_bytes -> marshal (mk_part i part_bytes))

(*****************************************************************************)
(* Perf event *)
(*****************************************************************************)

type Runtime_events.User.tag += Perf_event_tag

let perf_event_type =
  let encode (bytes : bytes) ((marshaled, len) : marshaled) : int =
    Bytes.blit marshaled 0 bytes 0 len;
    len
  in
  let decode (bytes : bytes) (len : int) : marshaled = (bytes, len) in
  Runtime_events.Type.register ~encode ~decode

let perf_event =
  Runtime_events.User.register "Perf_event" Perf_event_tag perf_event_type

let emit_point (p : point) =
  let marshaled_events = marshal_point p in
  List.iter
    (fun marshaled -> Runtime_events.User.write perf_event marshaled)
    marshaled_events
[@@inline always]

(* buffer for storing partial points so we can then rebuild them  *)
(* of type (ring_id, point parts) *)
type point_buffer = (int, (int * Bytes.t) list) Hashtbl.t

(** [event_of_perf_event ring_buffer_index buffer event] collects marshaled
    events, and re-assembles them into points. Since the points are split into
    parts, we return [None] if there was not enough parts to reconstruct a
    point, or [Some point] if there were.

    The runtime events file we read in has a unique ring buffer for each domain
    ([ring_buffer_index]). Events are written to this buffer in order. This means
    we can assume that the parts will come be read in order (e.g. ring 1 part 1,
    ring 1 part 2 ...). We collect these parts to form the final point. If we
    receive an out of order part, the last part in a point, or lose runtime events
    in a ring buffer, we reset our point buffer *)
let process_perf_event ring_buffer_index buffer (marshaled, _) : point option =
  let event = Marshal.from_bytes marshaled 0 in
  let ring_parts =
    match Hashtbl.find_opt buffer ring_buffer_index with
    | Some parts -> parts
    | None -> []
  in
  match event with
  (* Don't store in buffer if we can immediately unmarshal *)
  | { bytes; part_count; _ } when part_count = 1 ->
      (* Also clear out the buffer just in case *)
      Hashtbl.remove buffer ring_buffer_index;
      Some (Marshal.from_bytes bytes 0)
  (* If we don't have any parts, and receive something besides the start part,
     just wait for the next start part*)
  | { part; _ } when List.length ring_parts = 0 && part != 0 -> None
  (* If we already have some parts, or this is the start part, begin collecting parts *)
  | { bytes; part_count; _ } ->
      let parts = bytes :: ring_parts in
      let parts_len = List.length parts in
      (* If we have enough then unmarshal! *)
      (* TODO: We probably can just make the array all at once since we know the
         size in theory? *)
      if parts_len = part_count then (
        let full_bytes =
          List.fold_left
            (fun acc bytes ->
              let new_acc =
                Bytes.create (Bytes.length acc + Bytes.length bytes)
              in
              Bytes.blit acc 0 new_acc 0 (Bytes.length acc);
              Bytes.blit bytes 0 new_acc (Bytes.length acc) (Bytes.length bytes);
              new_acc)
            (Bytes.create 0) (List.rev parts)
        in
        Hashtbl.remove buffer ring_buffer_index;
        Some (Marshal.from_bytes full_bytes 0))
      else if parts_len > part_count then (
        (* Weird state, clear buffer *)
        Hashtbl.remove buffer ring_buffer_index;
        None)
      else (
        (* If not then update the buffer *)
        Hashtbl.replace buffer ring_buffer_index parts;
        None)
