(lang dune 3.2)

(name bio_io)

(generate_opam_files true)

(cram enable)

(version 0.7.0-SNAPSHOT)

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
  bisect_ppx
  (core
   (and
    (>= "v0.16")
    (< "v0.17")))
  (core_bench
   (and
    (>= "v0.16")
    (< "v0.17")))
  (core_unix
   (and
    (>= "v0.16")
    (< "v0.17")))
  (ocaml
   (>= "4.14"))
  (ocamlformat
   (and
    (>= "0.26")
    (< "0.27")))
  (ocaml-lsp-server
   (>= "1.11"))
  (ppx_sexp_conv
   (and
    (>= "v0.16")
    (< "v0.17")))))

(package
 (name bio_io)
 (synopsis
  "A library for reading and writing common file formats used in bioinformatics like FASTA files")
 (depends
  ; Runtime dependencies
  (base
   (and
    (>= "v0.16")
    (< "v0.17")))
  (ocaml
   (>= "4.14"))
  (ppx_compare
   (and
    (>= "v0.16")
    (< "v0.17")))
  (ppx_fields_conv
   (and
    (>= "v0.16")
    (< "v0.17")))
  (ppx_sexp_conv
   (and
    (>= "v0.16")
    (< "v0.17")))
  (stdio
   (and
    (>= "v0.16")
    (< "v0.17")))
  ; Test-only dependencies
  (ppx_assert
   (and
    (and
     (>= "v0.16")
     (< "v0.17"))
    :with-test))
  (base_quickcheck
   (and
    (and
     (>= "v0.16")
     (< "v0.17"))
    :with-test))
  (ppx_expect
   (and
    (and
     (>= "v0.16")
     (< "v0.17"))
    :with-test))
  (ppx_inline_test
   (and
    (and
     (>= "v0.16")
     (< "v0.17"))
    :with-test))
  (re
   (and
    (>= "1.10")
    :with-test))))
