(lang dune 2.8)

(generate_opam_files true)

(name bio_io)

(package
 (name bio_io)
 (version 0.2.0)
 (license "MIT")
 (maintainers "Ryan Moore")
 (authors "Ryan Moore")
 (homepage "https://github.com/mooreryan/bio_io")
 (source (github mooreryan/bio_io))
 (documentation "https://mooreryan.github.io/bio_io/")
 (bug_reports "https://github.com/mooreryan/bio_io/issues")
 (synopsis "A library for reading and writing common file formats used in bioinformatics like FASTA files")
 (depends
  ; Runtime dependencies
  (base (>= "v0.12"))
  (ppx_sexp_conv (>= "v0.12"))
  (ocaml (>= "4.08.0"))
  (stdio (>= "v0.12"))
  ; Development dependencies
  (bisect_ppx :dev)
  (core (and (>= "v0.12") :dev))
  (core_bench (and (>= "v0.12") :dev))
  ; Test-only dependencies
  (ppx_expect (and (>= "v0.12") :with-test))
  (ppx_inline_test (and (>= "v0.12") :with-test))))
