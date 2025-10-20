(lang dune 3.20)

(name pyro-caml)

(generate_opam_files true)

(source
 (github semgrep/pyro-caml))

(license LGPL-2.1-only)

(authors "Austin Theriault <austin@cutedogs.org>")

(maintainers "Austin Theriault <austin@cutedogs.org>")

(package
 (name pyro-caml-instruments)
 (synopsis "Pyroscope + OCaml = Pyro Caml")
 (depends ocaml mtime logs digestif)
 (tags ("profiling")))

(package
 (name pyro-caml-ppx)
 (synopsis "Pyro Caml PPX")
 (depends
  ocaml
  ; See https://github.com/LexiFi/landmarks/issues/44
  (ppxlib
   (<= 0.35.0))
  pyro-caml-instruments)
 (tags
  ("profiling" "ppx")))

(package
 (name pyro-caml)
 (synopsis "OCaml profiler with Pyroscope backend")
 (depends ocaml conf-rust-2024))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
