(lang dune 3.2)

(generate_opam_files true)

(cram enable)

(version 0.5.0-preview)

(maintainers "Ryan M. Moore")

(authors "Ryan M. Moore")

(homepage "https://github.com/mooreryan/bio_io")

(source
 (github mooreryan/bio_io))

(documentation "https://mooreryan.github.io/bio_io/")

(bug_reports "https://github.com/mooreryan/bio_io/issues")

(package
 (name bio_io-dev)
 (allow_empty)
 (depends
  (ocaml
   (>= "4.11"))
  bisect_ppx
  (core
   (>= "v0.15"))
  (core_bench
   (>= "v0.15"))
  (core_unix
   (>= "v0.15"))
  (ocamlformat
   (and
    (>= "0.22")
    (< "0.23")))
  (ocaml-lsp-server
   (>= "1.11"))
  (ppx_sexp_conv
   (>= "v0.15"))))

(package
 (name bio_io)
 (synopsis
  "A library for reading and writing common file formats used in bioinformatics like FASTA files")
 (depends
  ; Runtime dependencies
  (base
   (>= "v0.15"))
  (ocaml
   (>= "4.10"))
  (ppx_compare
   (>= "v0.15"))
  (ppx_fields_conv
   (>= "v0.15"))
  (ppx_sexp_conv
   (>= "v0.15"))
  (stdio
   (>= "v0.15"))
  ; Test-only dependencies
  (ppx_assert
   (and
    (>= "v0.15")
    :with-test))
  (base_quickcheck
   (and
    (>= "v0.15")
    :with-test))
  (ppx_expect
   (and
    (>= "v0.15")
    :with-test))
  (ppx_inline_test
   (and
    (>= "v0.15")
    :with-test))
  (re
   (and
    (>= "1.10")
    :with-test))))
