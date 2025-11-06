# Pyro Caml 🔥 🐪
Pyro Caml is a profiler for OCaml, that works with
[Pyroscope](https://pyroscope.io/) for statistical continuous profiling purely
in user space.

# How to use
Pyro Caml consists of three parts, the instrumentation library, the profiler,
and a helpful PPX. The instrumentation and PPX libraries has no dependencies on
Rust, so it is able to be used like any other OCaml library. The library does
take a hard requirement on OCaml's new runtime event system introduced in OCaml
5, so is incompatible with older versions.

## Instrumentation Library
With the library installed, add it as a dependency to a dune file, preferably
the one defining the executable/entrypoint of the program:

```

(executable
 (package example-package)
 (name example)
 (public_name example)
 (libraries 
   ;...
   pyro-caml-instruments
   ;...
   )
 (modes exe))
```

Then enable the profiler, again preferably at the start of the program:

``` ocaml
let ()=
  Pyro_caml_instruments.maybe_with_memprof_sampler
  @@ fun () ->
  (* ... *)
```

from here the profiler will run, but it will only write profile samples to its
ring buffer if the OCaml Runtime events runtime tracing is enabled. This can be
done by either setting the environment variable `OCAML_RUNTIME_EVENTS_START` or
calling `Runtime_events.start`. If you are using the Pyro Caml binary, this will
be done for you. 

Note that this library uses the builtin OCaml profiling engine `Memprof`, and
may cause issues if you are using the profil

Now your program is instrumented and can be used with the profiler!

## Profiler
First obtain a `pyro-caml` binary, either via building it yourself (see
[Development](#development)), or soon, opam or the release page.

The profiler sends profiles via Pyroscope, so before we can do any profiling, we
first need a Pyroscope instance. There are two easy ways to do this, the first
is just running Pyroscope via Docker locally:

``` sh
docker run -it -p 4040:4040 grafana/pyroscope
```

Then in another terminal, you can run the profiler:

``` sh
pyro-caml --address "http://localhost:4040" --name "my_ocaml_program"  <BINARY> [ARGS_TO_BINARY]...
```

Now navigate to [localhost:4040](http://localhost:4040), and select the service
in the top left corner, and switch it to whatever you passed as the name
argument.

This version of Pyroscope is missing some nice things, like diffs between profile ranges, tags, etc. Pyroscope deployed via a Grafana instance is much more full featured.

To set this up with Grafana, follow [this
guide](https://grafana.com/docs/grafana/latest/datasources/pyroscope/configure-pyroscope-data-source/)
to first add Pyroscope as a datasource. For now Pyro Caml only supports basic
auth. Get the url, basic auth username and password, and then run the profiler:

``` sh
PYRO_CAML_BASIC_AUTH_USERNAME="username" PYRO_CAML_BASIC_AUTH_PASSWORD="password" pyro-caml --address "https://profiles-prod-008.grafana.net"  --name "my_ocaml_program"  <BINARY> [ARGS_TO_BINARY]...
```

## PPX
Since the instrumentation works via the memprof profiler, that means places with
low #s of allocations may not generate samples. We provide a PPX to
automatically annotate top level functions to explicitly emit samples.

To annotate all top level functions of all modules in a dune library:

```
 (preprocess
  (pps ppx_pyro_caml --auto))
```

If you wish to turn of the auto annotation for a section of code, add `[@@@pyro_profile "auto-off"]`.


If you don't want to annotate all modules, you can specify specific sections with:

``` ocaml
[@@@pyro_profile "auto-on"]

let f = ...

(* optionally turn it off *)
[@@@pyro_profile "auto-off"]
```

Finally, for more fine grain control, you can write:

``` ocaml
let f = ... [@@pyro_profile]
(* or say *)

let () =
  let[@pyro_profile] rec even n = (n = 0) || odd (n - 1)
  and[@pyro_profile] odd n = (n = 1) || n > 0 && even (n - 1)
  in Printf.printf "'six is even' is %b\n" (even 6)
```

# Development

## Opam

``` sh
make setup
```

This creates an OCaml switch with the necessary dependencies. Note that Rust is
required for building the `pyro-caml` binary, but not the OCaml library.

## Nix

``` sh
make shell
```

This will create a nix development shell

## Building

``` sh
make build # builds the pyro-caml binary for development
make build-release # builds a faster release version
```
