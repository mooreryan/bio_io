open! Base
open Bio_io
module Filename = Caml.Filename
module In_channel = Stdio.In_channel
module Out_channel = Stdio.Out_channel

exception Exit

let print_endline = Stdio.print_endline
let printf = Stdio.printf
let eprintf = Stdio.eprintf
let sprintf = Printf.sprintf
let exit = Caml.exit
let raise_notrace = Caml.raise_notrace

let write_tmp_file data =
  let fname =
    Filename.concat
      (Filename.get_temp_dir_name ())
      "bio_io_test_fastq_in_channel.txt"
  in
  let () =
    match Caml.Sys.file_exists fname with
    | true -> Caml.Sys.remove fname
    | false -> ()
  in
  let chan = Out_channel.create fname in
  let () = Out_channel.output_string chan data in
  let () = Out_channel.flush chan in
  let () = Out_channel.close chan in
  (fname, chan)

let serialize r = Sexp.to_string_hum ~indent:1 @@ Fastq.Record.sexp_of_t r

let%expect_test "simple with_file_records" =
  let name, _chan = write_tmp_file Test_fastq_in_channel_data.seqs in
  let actual = Fastq.In_channel.with_file_records name in
  print_endline (Sexp.to_string_hum ([%sexp_of: Fastq.Record.t List.t] actual));
  [%expect
    {|
      (((id s1) (desc (apple#)) (seq ACTGN) (qual !!!!?) (extra (" a a   a%")))
       ((id s2) (desc (pie#)) (seq actgN) (qual !!!!?) (extra ("")))) |}]

let%expect_test "tricky with_file_records" =
  let name, _chan = write_tmp_file Test_fastq_in_channel_data.tricky_seqs in
  let actual = Fastq.In_channel.with_file_records name in
  print_endline
    (Sexp.to_string_hum ~indent:1 ([%sexp_of: Fastq.Record.t List.t] actual));
  [%expect
    {|
 (((id "") (desc ("empty seq at beginning#")) (seq "") (qual "") (extra ("")))
  ((id seq1) (desc ("is fun#")) (seq AACTGGAAN) (qual !!!!!!!!?) (extra ("")))
  ((id seq2#) (desc ()) (seq AATCCTGGGN) (qual !!!!!!!!!?) (extra ("")))
  ((id "") (desc ("empty seq 1#")) (seq "") (qual "") (extra ("")))
  ((id "") (desc ("empty seq 2#")) (seq "") (qual "") (extra ("")))
  ((id seq3#) (desc ()) (seq yyyyyyyyyyyyyyyyyN) (qual !!!!!!!!!!!!!!!!!?)
   (extra (seq3%)))
  ((id seq) (desc ("4 @ has many '@' in header#")) (seq ACTGactN)
   (qual 1234!@#?) (extra (" h h eeee%")))
  ((id seq) (desc (5#)) (seq actN) (qual 123?) (extra ("")))
  ((id empty) (desc ("seq at end")) (seq "") (qual "") (extra (""))))
  |}]

let%expect_test "general usage" =
  let open Fastq in
  let name, _chan = write_tmp_file Test_fastq_in_channel_data.seqs in
  In_channel.with_file_iteri_records name ~f:(fun i r ->
      printf "=== %d ===\n" i;
      Stdio.print_string @@ Record.to_string_nl r;
      printf "");
  [%expect
    {|
    === 0 ===
    @s1 apple#
    ACTGN
    + a a   a%
    !!!!?
    === 1 ===
    @s2 pie#
    actgN
    +
    !!!!? |}]

let%expect_test _ =
  let open Fastq in
  let name, _chan = write_tmp_file Test_fastq_in_channel_data.all_ampersand in
  let l = Or_error.try_with (fun () -> In_channel.with_file_records name) in
  print_endline @@ Sexp.to_string_hum
  @@ [%sexp_of: Fastq.Record.t List.t Or_error.t] l;
  [%expect
    {|
    (Error
     (Failure
      "I expected to see an extra line starting with '+', but saw '' instead.")) |}]

let%expect_test _ =
  let open Fastq in
  let name, _chan = write_tmp_file Test_fastq_in_channel_data.all_ampersand2 in
  let l = Or_error.try_with (fun () -> In_channel.with_file_records name) in
  print_endline @@ Sexp.to_string_hum
  @@ [%sexp_of: Fastq.Record.t List.t Or_error.t] l;
  [%expect
    {|
    (Error
     (Failure
      "I expected to see an extra line starting with '+', but saw '' instead.")) |}]

let%expect_test _ =
  let open Fastq in
  let name, _chan = write_tmp_file Test_fastq_in_channel_data.empty in
  let l = Or_error.try_with (fun () -> In_channel.with_file_records name) in
  print_endline @@ Sexp.to_string_hum
  @@ [%sexp_of: Fastq.Record.t List.t Or_error.t] l;
  [%expect {| (Ok ()) |}]

let%expect_test _ =
  let open Fastq in
  let name, _chan = write_tmp_file Test_fastq_in_channel_data.one_line in
  let l = Or_error.try_with (fun () -> In_channel.with_file_records name) in
  print_endline @@ Sexp.to_string_hum
  @@ [%sexp_of: Fastq.Record.t List.t Or_error.t] l;
  [%expect
    {|
    (Error
     (Failure "I was partway through a record, but I hit the end of the file.")) |}]

let%expect_test _ =
  let open Fastq in
  let name, _chan = write_tmp_file Test_fastq_in_channel_data.two_lines in
  let l = Or_error.try_with (fun () -> In_channel.with_file_records name) in
  print_endline @@ Sexp.to_string_hum
  @@ [%sexp_of: Fastq.Record.t List.t Or_error.t] l;
  [%expect
    {|
    (Error
     (Failure "I was partway through a record, but I hit the end of the file.")) |}]

let%expect_test _ =
  let open Fastq in
  let name, _chan = write_tmp_file Test_fastq_in_channel_data.three_lines in
  let l = Or_error.try_with (fun () -> In_channel.with_file_records name) in
  print_endline @@ Sexp.to_string_hum
  @@ [%sexp_of: Fastq.Record.t List.t Or_error.t] l;
  [%expect
    {|
    (Error
     (Failure "I was partway through a record, but I hit the end of the file.")) |}]

let%expect_test _ =
  let open Fastq in
  let name, _chan = write_tmp_file Test_fastq_in_channel_data.four_lines in
  let l = Or_error.try_with (fun () -> In_channel.with_file_records name) in
  print_endline @@ Sexp.to_string_hum
  @@ [%sexp_of: Fastq.Record.t List.t Or_error.t] l;
  [%expect
    {| (Ok (((id apple) (desc (pie)) (seq actgN) (qual ....?) (extra (""))))) |}]

let%expect_test _ =
  let open Fastq in
  let name, _chan =
    write_tmp_file Test_fastq_in_channel_data.four_lines_with_starting_blank
  in
  let l = Or_error.try_with (fun () -> In_channel.with_file_records name) in
  print_endline @@ Sexp.to_string_hum
  @@ [%sexp_of: Fastq.Record.t List.t Or_error.t] l;
  [%expect
    {|
    (Error
     (Failure
      "I expected to see a header line starting with '@', but saw '' instead.")) |}]

let%expect_test _ =
  let open Fastq in
  let name, _chan = write_tmp_file Test_fastq_in_channel_data.random_stuff in
  let l = Or_error.try_with (fun () -> In_channel.with_file_records name) in
  print_endline @@ Sexp.to_string_hum
  @@ [%sexp_of: Fastq.Record.t List.t Or_error.t] l;
  [%expect
    {|
    (Error
     (Failure
      "I expected to see a header line starting with '@', but saw '' instead.")) |}]

let%expect_test _ =
  let open Fastq in
  let name, _chan = write_tmp_file Test_fastq_in_channel_data.no_extra_marker in
  let l = Or_error.try_with (fun () -> In_channel.with_file_records name) in
  print_endline @@ Sexp.to_string_hum
  @@ [%sexp_of: Fastq.Record.t List.t Or_error.t] l;
  [%expect
    {|
    (Error
     (Failure
      "I expected to see an extra line starting with '+', but saw '' instead.")) |}]
