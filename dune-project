(lang dune 2.8)

(generate_opam_files true)

(name bio_io)

(package
 (name bio_io)
 (version 0.1.0)
 (license "MIT or Apache-2.0")
 (maintainers "Ryan Moore")
 (authors "Ryan Moore")
 (homepage "https://github.com/mooreryan/bio_io")
 (bug_reports "https://github.com/mooreryan/bio_io/issues")
 (synopsis "API for reading and writing common file formats used in bioinformatics like FASTA files")
 (depends core_kernel bisect_ppx))
