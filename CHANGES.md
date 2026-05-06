# Changes

## unreleased

- Relicense from LGPL-2.1-only to MIT.
- Add release automation: GitHub workflow builds pre-built binaries and creates a GitHub Release on `v*` tag pushes.
- Add `bump-version` workflow that opens a PR rolling `dune-project`, `CHANGES.md`, and the `*.opam` files for a new version.
- opam-repository submission is no longer done in CI — run `opam-publish` locally after the GitHub release is up. The `opam-publish` plugin is included in the Nix dev shell.
