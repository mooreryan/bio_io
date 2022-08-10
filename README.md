# Bio_io

[![Build and test](https://github.com/mooreryan/bio_io/actions/workflows/build_and_test.yml/badge.svg?branch=main)](https://github.com/mooreryan/bio_io/actions/workflows/build_and_test.yml) [![Coverage Status](https://coveralls.io/repos/github/mooreryan/bio_io/badge.svg?branch=main)](https://coveralls.io/github/mooreryan/bio_io?branch=main)

`Bio_io` is an OCaml library that provides programmatic access to
common file formats used in bioinformatics like FASTA files.

## Quick Start

### Install

```
opam install bio_io
```

### Example

Read a FASTA file and print the ID and sequence length for each record.

```ocaml
open! Base

let fasta_file = "sequences.fasta"

let () =
  (* This open gives you [In_channel] and [Record]. *)
  let open Bio_io.Fasta in
  In_channel.with_file_iter_records fasta_file ~f:(fun record ->
      (* Print the ID and the length of the sequence. *)
      Stdio.printf "%s => %d\n" (Record.id record) (Record.seq_length record))
```

## Docs

For more examples, API, and other usage info, see the [docs](https://mooreryan.github.io/bio_io/).

## License

[![license MIT or Apache
2.0](https://img.shields.io/badge/license-MIT%20or%20Apache%202.0-blue)](https://github.com/mooreryan/bio_io)

Licensed under the Apache License, Version 2.0 or the MIT license, at
your option. This program may not be copied, modified, or distributed
except according to those terms.
