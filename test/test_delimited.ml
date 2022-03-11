open! Core_kernel
open! Bio_io
module Q = Quickcheck

let trials =
  match Sys.getenv_opt "QC_TRIALS" with
  | None -> Q.default_trial_count
  | Some x -> Int.of_string x

let write_tmp_file data =
  let fname =
    Caml.Filename.concat
      (Caml.Filename.get_temp_dir_name ())
      "bio_io_test_ic.txt"
  in
  let () =
    match Caml.Sys.file_exists fname with
    | true -> Caml.Sys.remove fname
    | false -> ()
  in
  let chan = Stdio.Out_channel.create fname in
  Stdio.Out_channel.output_string chan data;
  Stdio.Out_channel.flush chan;
  Stdio.Out_channel.close chan;
  (fname, chan)

let print_string_s s = Stdio.print_s @@ String.sexp_of_t s

let%expect_test _ =
  let records = Mmseqs.In_channel.with_file_records "test_files/bad_btab.tsv" in
  print_s @@ [%sexp_of: Mmseqs.Record.t list Or_error.t] @@ records;
  [%expect {| (Error ("Caught exception" (Failure "Bad input"))) |}]

let%expect_test _ =
  let records = Btab.In_channel.with_file_records "test_files/bad_btab.tsv" in
  print_s @@ [%sexp_of: Btab.Record.t list Or_error.t] @@ records;
  [%expect {| (Error ("Caught exception" (Failure "Bad input"))) |}]

let%expect_test _ =
  let records = Mmseqs.In_channel.with_file_records_exn "test_files/btab.tsv" in
  print_s @@ [%sexp_of: Mmseqs.Record.t list] @@ records;
  [%expect
    {|
    (((query "Q 1") (target q1t1) (fident 0.1) (alnlen 2) (mismatch 3)
      (gapopen 4) (qstart 5) (qend 6) (tstart 7) (tend 8) (evalue 9.99E-05)
      (bits 10))
     ((query "Q 1") (target q1t2) (fident 0.11) (alnlen 12) (mismatch 13)
      (gapopen 14) (qstart 15) (qend 16) (tstart 17) (tend 18) (evalue 1.9E-05)
      (bits 20))
     ((query Q_2) (target q2t1) (fident 0.21) (alnlen 22) (mismatch 23)
      (gapopen 24) (qstart 25) (qend 26) (tstart 27) (tend 28) (evalue 2.9E-05)
      (bits 30))) |}]

let%expect_test _ =
  let records = Btab.In_channel.with_file_records_exn "test_files/btab.tsv" in
  print_s @@ [%sexp_of: Btab.Record.t list] @@ records;
  [%expect
    {|
    (((qaccver "Q 1") (saccver q1t1) (pident 0.1) (length 2) (mismatch 3)
      (gapopen 4) (qstart 5) (qend 6) (sstart 7) (send 8) (evalue 9.99E-05)
      (bitscore 10))
     ((qaccver "Q 1") (saccver q1t2) (pident 0.11) (length 12) (mismatch 13)
      (gapopen 14) (qstart 15) (qend 16) (sstart 17) (send 18) (evalue 1.9E-05)
      (bitscore 20))
     ((qaccver Q_2) (saccver q2t1) (pident 0.21) (length 22) (mismatch 23)
      (gapopen 24) (qstart 25) (qend 26) (sstart 27) (send 28) (evalue 2.9E-05)
      (bitscore 30))) |}]

let%expect_test "incomplete row" =
  let records = Or_error.try_with (fun () -> Mmseqs.Record.of_string "apple") in
  print_s @@ [%sexp_of: Mmseqs.Record.t Or_error.t] records;
  [%expect {| (Error (Failure "Bad input")) |}]

(* Property tests *)

let bad_stuff = Re2.create_exn "[\t\r\n]"

let has_bad_stuff s = Re2.matches bad_stuff s

let gen_string_no_separators =
  Q.Generator.filter String.quickcheck_generator ~f:(Fn.non has_bad_stuff)

(* Generate btab lines...the numbers won't always make sense but it has the
   correct number of columns. *)
let generate_valid_btab_line =
  let open Q.Generator in
  gen_string_no_separators >>= fun query ->
  gen_string_no_separators >>= fun target ->
  Float.quickcheck_generator >>= fun fident ->
  Int.quickcheck_generator >>= fun alnlen ->
  Int.quickcheck_generator >>= fun mismatch ->
  Int.quickcheck_generator >>= fun gapopen ->
  Int.quickcheck_generator >>= fun qstart ->
  Int.quickcheck_generator >>= fun qend ->
  Int.quickcheck_generator >>= fun tstart ->
  Int.quickcheck_generator >>= fun tend ->
  Float.quickcheck_generator >>= fun evalue ->
  Float.quickcheck_generator >>= fun bits ->
  Q.Generator.return
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
  let open Q.Generator in
  generate_valid_btab_line >>= fun btab ->
  Int.quickcheck_generator >>= fun qlen ->
  Int.quickcheck_generator >>= fun tlen ->
  Q.Generator.return
  @@ String.concat ~sep:"\t" [ btab; Int.to_string qlen; Int.to_string tlen ]

let%test_unit "Mmseqs.Record round tripping works" =
  Q.test ~trials generate_valid_btab_line ~sexp_of:String.sexp_of_t
    ~f:(fun btab_line ->
      let parsed =
        Mmseqs.Record.to_string @@ Mmseqs.Record.of_string btab_line
      in
      [%test_eq: string] parsed btab_line)

let%test_unit "Blast_6_record round tripping works" =
  Q.test ~trials generate_valid_btab_line ~sexp_of:String.sexp_of_t
    ~f:(fun btab_line ->
      let parsed = Btab.Record.to_string @@ Btab.Record.of_string btab_line in
      [%test_eq: string] parsed btab_line)

(* Let's make a custom in_channel. *)

module Silly_record = struct
  type t = { query : string; target : string } [@@deriving sexp]

  let of_string s =
    match String.split ~on:',' s with
    | [ query; target ] -> { query; target }
    | _ -> failwith "Bad record line"

  let to_string { query; target } = query ^ "," ^ target

  let generate_valid_line =
    let open Q.Generator in
    let bad_stuff = Re2.create_exn "[,\r\n]" in
    let has_bad_stuff s = Re2.matches bad_stuff s in
    let gen_string_no_separators =
      Q.Generator.filter String.quickcheck_generator ~f:(Fn.non has_bad_stuff)
    in
    gen_string_no_separators >>= fun query ->
    gen_string_no_separators >>= fun target ->
    Q.Generator.return (query ^ "," ^ target)
end

module Silly_in_channel : sig
  include Record_in_channel.S with type record := Silly_record.t
end = struct
  module T = struct
    include Private.In_channel

    type record = Silly_record.t

    let input_record_exn ic =
      Option.map ~f:Silly_record.of_string @@ Stdio.In_channel.input_line ic
  end

  include T
  include Record_in_channel.Make (T)
end

let%test_unit "Silly_record round tripping works" =
  Q.test ~trials Silly_record.generate_valid_line ~sexp_of:String.sexp_of_t
    ~f:(fun line ->
      let parsed = Silly_record.to_string @@ Silly_record.of_string line in
      [%test_eq: string] parsed line)

let%expect_test "illegal atom" =
  let records =
    Or_error.try_with (fun () -> Silly_record.of_string "apple\t2\t3\t4\t5")
  in
  print_s @@ [%sexp_of: Silly_record.t Or_error.t] records;
  [%expect {| (Error (Failure "Bad record line")) |}]
