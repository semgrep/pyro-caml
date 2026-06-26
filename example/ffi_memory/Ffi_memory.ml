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

(* In Pyroscope's alloc_space, search for these frames:
     ffi          -> tracked     (caml_alloc, OCaml heap)
     malloc_naked -> ~nothing    (off-heap, no OCaml object)
     custom_plain -> ~nothing    (only the wrapper is on-heap)
     custom_mem   -> tracked     (off-heap size declared via caml_alloc_custom_mem)

   ffi: 100k x (8 + 1) words x 8B = 7.2 MB which means ≈ 72 GB (~67 GiB) over 10k rounds
   custom_mem: 256 KiB ≈ 2.6 GB (~2.4 GiB) over 10k rounds
*)

external alloc_ocaml_blocks : int -> int -> unit = "ml_test_alloc_ocaml_blocks"
external alloc_malloc_naked : int -> int -> unit = "ml_test_alloc_malloc_naked"

type buf

external alloc_malloc_custom : int -> bool -> buf = "ml_test_alloc_malloc_custom"

let chunk = 256 * 1024

let ffi () = alloc_ocaml_blocks 100_000 8
let malloc_naked () = alloc_malloc_naked 32 chunk
let custom_plain () = ignore (alloc_malloc_custom chunk false)
let custom_mem () = ignore (alloc_malloc_custom chunk true)

let () =
  Pyro_caml_instruments.with_memprof_sampler @@ fun () ->
  Printf.printf "Starting loop\n%!";
  for _ = 1 to 10000 do
    ffi ();
    malloc_naked ();
    custom_plain ();
    custom_mem ()
  done;
  Printf.printf "Finished loop\n";

