(lang dune 3.2)

(name bio_io)

(generate_opam_files true)

(cram enable)

(version 0.7.0)

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
   (>= "v0.16"))
  (core_bench
   (>= "v0.16"))
  (core_unix
   (>= "v0.16"))
  (ocaml
   (>= "4.14"))
  (ocamlformat
   (= "0.26.2"))
  (ocaml-lsp-server
   (>= "1.11"))
  (ppx_sexp_conv
   (>= "v0.16"))))

(package
 (name bio_io)
 (synopsis
  "A library for reading and writing common file formats used in bioinformatics like FASTA files")
 (depends
  ; Runtime dependencies
  (base
   (>= "v0.16"))
  (ocaml
   (>= "4.14"))
  (ppx_compare
   (>= "v0.16"))
  (ppx_fields_conv
   (>= "v0.16"))
  (ppx_sexp_conv
   (>= "v0.16"))
  (stdio
   (>= "v0.16"))
  ; Test-only dependencies
  (ppx_assert
   (and
    (>= "v0.16")
    :with-test))
  (base_quickcheck
   (and
    (>= "v0.16")
    :with-test))
  (ppx_expect
   (and
    (>= "v0.16")
    :with-test))
  (ppx_inline_test
   (and
    (>= "v0.16")
    :with-test))
  (re
   (and
    (>= "1.10")
    :with-test))))
