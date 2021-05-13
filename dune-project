(lang dune 2.8)

(generate_opam_files true)

(package
 (name bio_io)
 (license "MIT or Apache-2.0")
 (maintainers "Ryan Moore")
 (authors "Ryan Moore")
 (synopsis "API for reading and writing common file formats used in bioinformatics like FASTA files")
 (depends core_kernel bisect_ppx))
