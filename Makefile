.PHONY: all vendor build build-release clean format setup install-deps shell release
all: build

vendor:
	cargo update
	cargo vendor --locked

build:
	dune build
	cargo build

build-release:
	dune build --profile=release
	cargo build --release

clean:
	dune clean
	cargo clean

format:
	dune build @fmt --auto-promote
	cargo fmt -- --check

# Should install rust also!
setup:
	opam update -y
	opam switch create . --with-dev-setup -y

install-deps:
	opam install . --deps-only --with-dev-setup -y

shell:
	nix develop .

release: vendor build-release
