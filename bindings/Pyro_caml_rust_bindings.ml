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

let read_poll cursor interval = Pyro_caml_instruments.read_poll cursor interval

let create_cursor = Pyro_caml_instruments.create_cursor

let () =
  Callback.register "read_poll_ml" read_poll ;
  Callback.register "create_cursor_ml" create_cursor
