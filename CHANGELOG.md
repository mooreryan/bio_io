## Unreleased

## 0.5.0 (2022-07-13)

### Added

- Add `Btab_queries` module to process Btab files query-by-query rather than hit-by-hit

### Changed

#### Delimited parsing

- Change record names in the `Btab` module (breaking change)
- `Btab.Record.t` is now abstract (breaking change)
- No `sexp` functions on `Btab.Record.t` (breaking change)
- Drop the `Mmseqs` module (breaking change)
- Speed up Btab module

#### Other

- Drop `core_kernel` from the tests, add `base_quickcheck` instead
- Drop `bisect_ppx`, `core`, and `core_bench` from `opam` file. These are now in the `bio_io-dev.opam` file.
- Drop `re2` in favor of `re`
- Update `dune` to `3.2`
- Update Jane Street libraries to `v0.15`
- Update `ocamlformat` to `0.22.4`

## 0.4.0 (2021-12-19)

### Fixed

- [Fasta.In_channel] can now read from fifos...e.g., you can pipe the output of `gunzip -c` directly into a program using the fasta parser and it will be okay.

### Added

- Added modules for MMseqs2 and BLAST (e.g., BLAST 6 tab-delimited output)
- Added a couple of example programs in the `examples` directory

### Changed

- `Fasta_in_channel` is now `Fasta.In_channel` (breaking change)
- `Fasta_record` is now `Fasta.Record` (breaking change)
- Various non-breaking changes

## 0.3.0 (2021-09-26)

- Add `Cigar` module for parsing CIGAR strings.

## 0.2.1 (2021-09-12)

- Specify `ppx_expect` and `ppx_inline_test` as test dependencies rather than dev dependencies.

## 0.2.0 (2021-09-11)

- Require `Base` rather than `Core_kernel`
- Add `seq_length` and `to_string_nl` functions to `Fasta_record` module

## 0.1.2 (2021-08-04)

- Added some benchmarks.
- Sped up fasta parsing.

## 0.1.1 (2021-05-15)

- Fixed the OCaml min. dependency to 4.08.
- Fixed the lint errors in the opam-ci

## 0.1.0 (2021-05-14)

Initial release!
