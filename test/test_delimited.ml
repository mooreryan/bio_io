open! Base
open! Bio_io
module Q = Base_quickcheck
module QG = Base_quickcheck.Generator

let print_s = Stdio.print_s
let print_endline = Stdio.print_endline

(* 10_000 is the default test count in base_quickcheck v0.15 *)
let trials =
  match Sys.getenv "QC_TRIALS" with None -> 10_000 | Some x -> Int.of_string x

let write_tmp_file data =
  let fname =
    Stdlib.Filename.concat
      (Stdlib.Filename.get_temp_dir_name ())
      "bio_io_test_ic.txt"
  in
  let () =
    match Stdlib.Sys.file_exists fname with
    | true -> Stdlib.Sys.remove fname
    | false -> ()
  in
  let chan = Stdio.Out_channel.create fname in
  Stdio.Out_channel.output_string chan data;
  Stdio.Out_channel.flush chan;
  Stdio.Out_channel.close chan;
  (fname, chan)

let print_string_s s = Stdio.print_s @@ String.sexp_of_t s

let%expect_test "bad Btab" =
  let records =
    Or_error.try_with (fun () ->
        Btab.In_channel.with_file_records "test_files/bad_btab.tsv")
  in
  Stdio.print_s @@ [%sexp_of: Btab.Record.t list Or_error.t] records;
  [%expect
    {|
    (Error (Failure "Bad btab line: 'this is a bad btab file'")) |}]

let%expect_test "bad Btab.queries" =
  let records =
    Or_error.try_with (fun () ->
        Btab_queries.In_channel.with_file_records "test_files/bad_btab.tsv")
  in
  Stdio.print_s @@ [%sexp_of: Btab_queries.Record.t list Or_error.t] records;
  [%expect
    {|
    (Error (Failure "Bad btab line: 'this is a bad btab file'")) |}]

let%expect_test _ =
  let records = Btab.In_channel.with_file_records "test_files/btab.tsv" in
  let records = List.map records ~f:Btab.Record.parse in
  print_s @@ [%sexp_of: Btab.Record.Parsed.t list] @@ records;
  [%expect
    {|
    (((query "Q 1") (target q1t1) (pident 0.1) (alnlen 2) (mismatch 3)
      (gapopen 4) (qstart 5) (qend 6) (tstart 7) (tend 8) (evalue 9.99E-05)
      (bits 10) (qlen ()) (tlen ()))
     ((query "Q 1") (target q1t2) (pident 0.11) (alnlen 12) (mismatch 13)
      (gapopen 14) (qstart 15) (qend 16) (tstart 17) (tend 18) (evalue 1.9E-05)
      (bits 20) (qlen ()) (tlen ()))
     ((query Q_2) (target q2t1) (pident 0.21) (alnlen 22) (mismatch 23)
      (gapopen 24) (qstart 25) (qend 26) (tstart 27) (tend 28) (evalue 2.9E-05)
      (bits 30) (qlen ()) (tlen ()))) |}]

let%expect_test _ =
  let records =
    Btab_queries.In_channel.with_file_records "test_files/btab.tsv"
  in
  let records =
    List.map records ~f:(fun record ->
        let hits =
          List.map ~f:Btab.Record.parse @@ Btab_queries.Record.hits record
        in
        (Btab_queries.Record.query record, hits))
  in
  print_s @@ [%sexp_of: (string * Btab.Record.Parsed.t list) list] @@ records;
  [%expect
    {|
    (("Q 1"
      (((query "Q 1") (target q1t1) (pident 0.1) (alnlen 2) (mismatch 3)
        (gapopen 4) (qstart 5) (qend 6) (tstart 7) (tend 8) (evalue 9.99E-05)
        (bits 10) (qlen ()) (tlen ()))
       ((query "Q 1") (target q1t2) (pident 0.11) (alnlen 12) (mismatch 13)
        (gapopen 14) (qstart 15) (qend 16) (tstart 17) (tend 18) (evalue 1.9E-05)
        (bits 20) (qlen ()) (tlen ()))))
     (Q_2
      (((query Q_2) (target q2t1) (pident 0.21) (alnlen 22) (mismatch 23)
        (gapopen 24) (qstart 25) (qend 26) (tstart 27) (tend 28) (evalue 2.9E-05)
        (bits 30) (qlen ()) (tlen ()))))) |}]

let%expect_test _ =
  Btab_queries.In_channel.with_file_iter_records "test_files/btab.tsv"
    ~f:(fun r ->
      print_endline "===";
      print_endline @@ Btab_queries.Record.query r;
      let hits = List.map ~f:Btab.Record.parse @@ Btab_queries.Record.hits r in
      print_s @@ [%sexp_of: Btab.Record.Parsed.t list] hits);
  [%expect
    {|
    ===
    Q 1
    (((query "Q 1") (target q1t1) (pident 0.1) (alnlen 2) (mismatch 3)
      (gapopen 4) (qstart 5) (qend 6) (tstart 7) (tend 8) (evalue 9.99E-05)
      (bits 10) (qlen ()) (tlen ()))
     ((query "Q 1") (target q1t2) (pident 0.11) (alnlen 12) (mismatch 13)
      (gapopen 14) (qstart 15) (qend 16) (tstart 17) (tend 18) (evalue 1.9E-05)
      (bits 20) (qlen ()) (tlen ())))
    ===
    Q_2
    (((query Q_2) (target q2t1) (pident 0.21) (alnlen 22) (mismatch 23)
      (gapopen 24) (qstart 25) (qend 26) (tstart 27) (tend 28) (evalue 2.9E-05)
      (bits 30) (qlen ()) (tlen ()))) |}]

(* Property tests *)

let bad_stuff = Re.Perl.compile_pat "[\t\r\n]"
let has_bad_stuff s = Re.execp bad_stuff s
let gen_string_no_separators = QG.filter QG.string ~f:(Fn.non has_bad_stuff)

(* Generate btab lines...the numbers won't always make sense but it has the
   correct number of columns. *)
let generate_valid_btab_line =
  let open QG in
  gen_string_no_separators >>= fun query ->
  gen_string_no_separators >>= fun target ->
  QG.float >>= fun fident ->
  QG.int >>= fun alnlen ->
  QG.int >>= fun mismatch ->
  QG.int >>= fun gapopen ->
  QG.int >>= fun qstart ->
  QG.int >>= fun qend ->
  QG.int >>= fun tstart ->
  QG.int >>= fun tend ->
  QG.float >>= fun evalue ->
  QG.float >>= fun bits ->
  QG.return
  @@ String.concat ~sep:"\t"
       [
         query;
         target;
         Float.to_string fident;
         Int.to_string alnlen;
         Int.to_string mismatch;
         Int.to_string gapopen;
         Int.to_string qstart;
         Int.to_string qend;
         Int.to_string tstart;
         Int.to_string tend;
         Float.to_string evalue;
         Float.to_string bits;
       ]

let generate_valid_btab_with_len_line =
  let open QG in
  generate_valid_btab_line >>= fun btab ->
  QG.int >>= fun qlen ->
  QG.int >>= fun tlen ->
  QG.return
  @@ String.concat ~sep:"\t" [ btab; Int.to_string qlen; Int.to_string tlen ]

let%test_unit "Btab round tripping works" =
  let config = { Q.Test.default_config with test_count = trials } in
  let f btab_line =
    let parsed = Btab.Record.to_string @@ Btab.Record.of_string btab_line in
    [%test_eq: string] parsed btab_line
  in
  Q.Test.run_exn ~config ~f
    (module struct
      type t = string [@@deriving sexp]

      let quickcheck_generator = generate_valid_btab_line
      let quickcheck_shrinker = Q.Shrinker.atomic
    end)

(* The original (slower, but simpler) implementation matches the new version. *)
let%test_unit "New btab parsers matches old btab parser" =
  let equal a b =
    let module A = Btab_orig.Record in
    let module B = Btab.Record in
    String.(A.query a = B.query b && A.target a = B.target b)
    && Robust.(
         A.pident a = B.pident b
         && A.evalue a = B.evalue b
         && A.bits a = B.bits b)
    && Int.(
         A.alnlen a = B.alnlen b
         && A.mismatch a = B.mismatch b
         && A.gapopen a = B.gapopen b
         && A.qstart a = B.qstart b
         && A.qend a = B.qend b
         && A.tstart a = B.tstart b
         && A.qend a = B.qend b)
    && Option.equal Int.equal (A.qlen a) (B.qlen b)
    && Option.equal Int.equal (A.tlen a) (B.tlen b)
  in
  let config = { Q.Test.default_config with test_count = trials } in
  let f btab_line =
    let a = Btab_orig.Record.of_string btab_line in
    let b = Btab.Record.of_string btab_line in
    assert (equal a b)
  in
  Q.Test.run_exn ~config ~f
    (module struct
      type t = string [@@deriving sexp]

      let quickcheck_generator = generate_valid_btab_with_len_line
      let quickcheck_shrinker = Q.Shrinker.atomic
    end)

(* Let's make a custom in_channel. *)

module Silly_record = struct
  type t = { query : string; target : string } [@@deriving sexp]

  let of_string s =
    match String.split ~on:',' s with
    | [ query; target ] -> { query; target }
    | _ -> failwith "Bad record line"

  let to_string { query; target } = query ^ "," ^ target

  let generate_valid_line =
    let open QG in
    let bad_stuff = Re.Perl.compile_pat "[,\r\n]" in
    let has_bad_stuff s = Re.execp bad_stuff s in
    let gen_string_no_separators =
      QG.filter QG.string ~f:(Fn.non has_bad_stuff)
    in
    gen_string_no_separators >>= fun query ->
    gen_string_no_separators >>= fun target -> QG.return (query ^ "," ^ target)
end

module Silly_in_channel : sig
  include Record_in_channel.S with type record := Silly_record.t
end = struct
  module T = struct
    include Private.In_channel

    type record = Silly_record.t

    let input_record ic =
      Option.map ~f:Silly_record.of_string @@ Stdio.In_channel.input_line ic
  end

  include T
  include Record_in_channel.Make (T)
end

let%test_unit "Silly_record round tripping works" =
  let config = { Q.Test.default_config with test_count = trials } in
  let f line =
    let parsed = Silly_record.to_string @@ Silly_record.of_string line in
    [%test_eq: string] parsed line
  in
  Q.Test.run_exn ~config ~f
    (module struct
      type t = string [@@deriving sexp]

      let quickcheck_generator = Silly_record.generate_valid_line
      let quickcheck_shrinker = Q.Shrinker.atomic
    end)

let%expect_test "illegal atom" =
  let records =
    Or_error.try_with (fun () -> Silly_record.of_string "apple\t2\t3\t4\t5")
  in
  print_s @@ [%sexp_of: Silly_record.t Or_error.t] records;
  [%expect {| (Error (Failure "Bad record line")) |}]

module Btab_example = struct
  module A = Bio_io.Btab.Record
  module B = Btab_orig.Record

  let s =
    "Q \
     1\tq1t1\t0.1\t22\t333\t4444\t55555\t666666\t7777777\t88888888\t9.99E-05\t1000"

  (* No lengths. *)

  let a = A.of_string s
  let b = B.of_string s

  let%test_unit "query" = [%test_eq: string] (A.query a) (B.query b)
  let%test_unit "target" = [%test_eq: string] (A.target a) (B.target b)
  let%test_unit "pident" = [%test_eq: float] (A.pident a) (B.pident b)
  let%test_unit "alnlen" = [%test_eq: int] (A.alnlen a) (B.alnlen b)
  let%test_unit "mismatch" = [%test_eq: int] (A.mismatch a) (B.mismatch b)
  let%test_unit "gapopen" = [%test_eq: int] (A.gapopen a) (B.gapopen b)
  let%test_unit "qstart" = [%test_eq: int] (A.qstart a) (B.qstart b)
  let%test_unit "qend" = [%test_eq: int] (A.qend a) (B.qend b)
  let%test_unit "tstart" = [%test_eq: int] (A.tstart a) (B.tstart b)
  let%test_unit "tend" = [%test_eq: int] (A.tend a) (B.tend b)
  let%test_unit "evalue" = [%test_eq: float] (A.evalue a) (B.evalue b)
  let%test_unit "bits" = [%test_eq: float] (A.bits a) (B.bits b)
  let%test_unit "qlen" = [%test_eq: int option] (A.qlen a) (B.qlen b)
  let%test_unit "qlen" = [%test_eq: int option] (A.tlen a) (B.tlen b)

  (* With lengths. *)

  let s =
    "Q \
     1\tq1t1\t0.1\t22\t333\t4444\t55555\t666666\t7777777\t88888888\t9.99E-05\t1000\t111\t2222"

  let a = A.of_string s
  let b = B.of_string s

  let%test_unit "query" = [%test_eq: string] (A.query a) (B.query b)
  let%test_unit "target" = [%test_eq: string] (A.target a) (B.target b)
  let%test_unit "pident" = [%test_eq: float] (A.pident a) (B.pident b)
  let%test_unit "alnlen" = [%test_eq: int] (A.alnlen a) (B.alnlen b)
  let%test_unit "mismatch" = [%test_eq: int] (A.mismatch a) (B.mismatch b)
  let%test_unit "gapopen" = [%test_eq: int] (A.gapopen a) (B.gapopen b)
  let%test_unit "qstart" = [%test_eq: int] (A.qstart a) (B.qstart b)
  let%test_unit "qend" = [%test_eq: int] (A.qend a) (B.qend b)
  let%test_unit "tstart" = [%test_eq: int] (A.tstart a) (B.tstart b)
  let%test_unit "tend" = [%test_eq: int] (A.tend a) (B.tend b)
  let%test_unit "evalue" = [%test_eq: float] (A.evalue a) (B.evalue b)
  let%test_unit "bits" = [%test_eq: float] (A.bits a) (B.bits b)
  let%test_unit "qlen" = [%test_eq: int option] (A.qlen a) (B.qlen b)
  let%test_unit "qlen" = [%test_eq: int option] (A.tlen a) (B.tlen b)
end
