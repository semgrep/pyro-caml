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
module Hash = Digest.MD5

type t =
  | Point of (int64 * Stack_trace.raw_stack_trace)
  | Partial of {id: Hash.t; bytes: Bytes.t; part: int; part_count: int}

type marshaled = bytes * int

let make_partial id part_count part bytes = Partial {id; bytes; part; part_count}

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
  let marshaled_event = Marshal.to_bytes e [] in
  let len = Bytes.length marshaled_event in
  (marshaled_event, len)

let marshal_event ?(max_size = 800) e =
  let marshaled_event, len = marshal e in
  if len <= 1024 then [(marshaled_event, len)]
  else
    let id = Hash.bytes marshaled_event in
    let parts = split_bytes marshaled_event max_size in
    let mk_part part part_bytes =
      make_partial id (List.length parts) part part_bytes
    in
    parts |> List.mapi (fun i part_bytes -> marshal (mk_part i part_bytes))

(*****************************************************************************)
(* Perf event *)
(*****************************************************************************)

type Runtime_events.User.tag += Perf_event_tag

let perf_event_type =
  let encode (bytes : bytes) ((marshaled, len) : marshaled) : int =
    Bytes.blit marshaled 0 bytes 0 len ;
    len
  in
  let decode (bytes : bytes) (len : int) : marshaled = (bytes, len) in
  Runtime_events.Type.register ~encode ~decode

let perf_event =
  Runtime_events.User.register "Perf_event" Perf_event_tag perf_event_type

let emit_event e =
  let marshaled_events = marshal_event e in
  List.iter
    (fun marshaled -> Runtime_events.User.write perf_event marshaled)
    marshaled_events
[@@inline always]

type event_buffer = (Hash.t, (int * Bytes.t) list) Hashtbl.t

let event_of_perf_event buffer (marshaled, _) : t =
  let event = Marshal.from_bytes marshaled 0 in
  match event with
  | Partial {id; bytes; part_count; part} ->
      let parts =
        match Hashtbl.find_opt buffer id with
        | Some parts ->
            (part, bytes) :: parts
        | None ->
            [(part, bytes)]
      in
      let parts =
        List.sort_uniq (fun (id1, _) (id2, _) -> Int.compare id1 id2) parts
      in
      if List.length parts = part_count then (
        let full_bytes =
          parts |> List.map snd
          |> List.fold_left
               (fun acc bytes ->
                 let new_acc =
                   Bytes.create (Bytes.length acc + Bytes.length bytes)
                 in
                 Bytes.blit acc 0 new_acc 0 (Bytes.length acc) ;
                 Bytes.blit bytes 0 new_acc (Bytes.length acc)
                   (Bytes.length bytes) ;
                 new_acc )
               (Bytes.create 0)
        in
        Hashtbl.remove buffer id ;
        Marshal.from_bytes full_bytes 0 )
      else (
        Hashtbl.replace buffer id parts ;
        event )
  | _ ->
      event
