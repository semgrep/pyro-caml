(lang dune 3.20)

(name pyro_caml)

(generate_opam_files true)

(source
 (github semgrep/pyro_caml))

(authors "Austin Theriault <austin@cutedogs.org>")

(package
 (name pyro-caml)
 (synopsis "Pyroscope + OCaml = Pyro Caml")
 (depends ocaml mtime logs digestif)
 (tags ("profiling")))

(package
 (name pyro-caml-rust-bindings)
 (synopsis "Pyro Caml bindings for rust")
 (depends ocaml pyro-caml conf-rust)
 (tags ("profiling")))

(package
 (name pyro-caml-ppx)
 (synopsis "Pyro Caml PPX")
 (depends ocaml ppxlib pyro-caml)
 (tags
  ("profiling" "ppx")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
