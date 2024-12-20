(* Note: most of the excersising of the Record_in_chanel is here. In other
   modules created with the functor, you mainly just need to test if the record
   list works as it should. *)

open! Base
open Bio_io
module Filename = Stdlib.Filename
module In_channel = Stdio.In_channel
module Out_channel = Stdio.Out_channel

exception Exit

let print_endline = Stdio.print_endline

let printf = Stdio.printf

let eprintf = Stdio.eprintf

let sprintf = Printf.sprintf

let exit = Stdlib.exit

let raise_notrace = Stdlib.raise_notrace

(* This one works with core kernel. *)
let write_tmp_file data =
  let fname =
    Filename.concat
      (Filename.get_temp_dir_name ())
      "bio_io_test_fasta_in_channel.txt"
  in
  let () =
    match Stdlib.Sys.file_exists fname with
    | true ->
        Stdlib.Sys.remove fname
    | false ->
        ()
  in
  let chan = Out_channel.create fname in
  let () = Out_channel.output_string chan data in
  let () = Out_channel.flush chan in
  let () = Out_channel.close chan in
  (fname, chan)

let serialize r = Sexp.to_string_hum ~indent:1 @@ Fasta.Record.sexp_of_t r

let%expect_test _ =
  let () =
    match Fasta.In_channel.create "ashoetnaoshntoasehnt" with
    | exception e ->
        Stdio.print_endline @@ Exn.to_string e
    | _ ->
        assert false
  in
  [%expect
    {|
    (Sys_error "ashoetnaoshntoasehnt: No such file or directory")
  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.seqs in
  let chan = Fasta.In_channel.create name in
  let closed = Fasta.In_channel.close chan in
  print_endline @@ Sexp.to_string_hum ([%sexp_of: unit] closed) ;
  [%expect {| () |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.seqs in
  let expected =
    Fasta.In_channel.with_file name ~f:(fun chan ->
        let record = Fasta.In_channel.input_record chan in
        (* For some reason the type inference is weird with Option.value_exn *)
        Option.value_exn record )
  in
  Stdio.print_s @@ [%sexp_of: Fasta.Record.t] expected ;
  [%expect {| ((id s1) (desc (apple)) (seq ACTGn)) |}]

let%expect_test "simple fold" =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.seqs in
  let actual =
    Fasta.In_channel.with_file_fold_records name ~init:"" ~f:(fun acc record ->
        acc ^ Fasta.Record.to_string_nl record )
  in
  print_endline actual ; [%expect {|
>s1 apple
ACTGn
>s2 pie
actgn
  |}]

let%expect_test "tricky fold" =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.tricky_seqs in
  let actual =
    Fasta.In_channel.with_file_fold_records name ~init:"" ~f:(fun acc record ->
        acc ^ Fasta.Record.to_string_nl record ~nl:"\n" )
  in
  print_endline actual ;
  [%expect
    {|
> empty seq at beginning

>seq1 is fun
AACTGGNNN
>seq2
AATCCTGNNN
> empty seq 1

> empty seq 2

>seq3
yyyyyyyyyyyyyyyNNN
>seq 4 > has many '>' in header
ACTGactg
>seq 5
actG
>empty seq at end
  |}]

let%expect_test "simple with_file_records" =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.seqs in
  let actual = Fasta.In_channel.with_file_records name in
  print_endline (Sexp.to_string_hum ([%sexp_of: Fasta.Record.t List.t] actual)) ;
  [%expect
    {| (((id s1) (desc (apple)) (seq ACTGn)) ((id s2) (desc (pie)) (seq actgn))) |}]

let%expect_test "tricky with_file_records" =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.tricky_seqs in
  let actual = Fasta.In_channel.with_file_records name in
  print_endline
    (Sexp.to_string_hum ~indent:1 ([%sexp_of: Fasta.Record.t List.t] actual)) ;
  [%expect
    {|
 (((id "") (desc ("empty seq at beginning")) (seq ""))
  ((id seq1) (desc ("is fun")) (seq AACTGGNNN))
  ((id seq2) (desc ()) (seq AATCCTGNNN))
  ((id "") (desc ("empty seq 1")) (seq ""))
  ((id "") (desc ("empty seq 2")) (seq ""))
  ((id seq3) (desc ()) (seq yyyyyyyyyyyyyyyNNN))
  ((id seq) (desc ("4 > has many '>' in header")) (seq ACTGactg))
  ((id seq) (desc (5)) (seq actG))
  ((id empty) (desc ("seq at end")) (seq "")))
  |}]

let%expect_test "tricky with_file_records" =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.tricky_seqs in
  let actual =
    Fasta.In_channel.with_file name ~f:(fun chan ->
        Fasta.In_channel.records chan )
  in
  print_endline
    (Sexp.to_string_hum ~indent:1 ([%sexp_of: Fasta.Record.t List.t] actual)) ;
  [%expect
    {|

 (((id "") (desc ("empty seq at beginning")) (seq ""))
  ((id seq1) (desc ("is fun")) (seq AACTGGNNN))
  ((id seq2) (desc ()) (seq AATCCTGNNN))
  ((id "") (desc ("empty seq 1")) (seq ""))
  ((id "") (desc ("empty seq 2")) (seq ""))
  ((id seq3) (desc ()) (seq yyyyyyyyyyyyyyyNNN))
  ((id seq) (desc ("4 > has many '>' in header")) (seq ACTGactg))
  ((id seq) (desc (5)) (seq actG))
  ((id empty) (desc ("seq at end")) (seq "")))
  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.tricky_seqs in
  let result =
    Fasta.In_channel.with_file name ~f:(fun chan ->
        Fasta.In_channel.fold_records chan ~init:"" ~f:(fun result record ->
            result ^ sprintf "%s\n" (serialize record) ) )
  in
  print_endline result ;
  [%expect
    {|
    ((id "") (desc ("empty seq at beginning")) (seq ""))
    ((id seq1) (desc ("is fun")) (seq AACTGGNNN))
    ((id seq2) (desc ()) (seq AATCCTGNNN))
    ((id "") (desc ("empty seq 1")) (seq ""))
    ((id "") (desc ("empty seq 2")) (seq ""))
    ((id seq3) (desc ()) (seq yyyyyyyyyyyyyyyNNN))
    ((id seq) (desc ("4 > has many '>' in header")) (seq ACTGactg))
    ((id seq) (desc (5)) (seq actG))
    ((id empty) (desc ("seq at end")) (seq ""))

  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.tricky_seqs in
  let result =
    Fasta.In_channel.with_file_fold_records name ~init:""
      ~f:(fun result record -> result ^ sprintf "%s\n" (serialize record) )
  in
  print_endline result ;
  [%expect
    {|
    ((id "") (desc ("empty seq at beginning")) (seq ""))
    ((id seq1) (desc ("is fun")) (seq AACTGGNNN))
    ((id seq2) (desc ()) (seq AATCCTGNNN))
    ((id "") (desc ("empty seq 1")) (seq ""))
    ((id "") (desc ("empty seq 2")) (seq ""))
    ((id seq3) (desc ()) (seq yyyyyyyyyyyyyyyNNN))
    ((id seq) (desc ("4 > has many '>' in header")) (seq ACTGactg))
    ((id seq) (desc (5)) (seq actG))
    ((id empty) (desc ("seq at end")) (seq ""))

  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.tricky_seqs in
  let _ =
    Fasta.In_channel.with_file_iter_records name ~f:(fun record ->
        print_endline (serialize record) )
  in
  [%expect
    {|
    ((id "") (desc ("empty seq at beginning")) (seq ""))
    ((id seq1) (desc ("is fun")) (seq AACTGGNNN))
    ((id seq2) (desc ()) (seq AATCCTGNNN))
    ((id "") (desc ("empty seq 1")) (seq ""))
    ((id "") (desc ("empty seq 2")) (seq ""))
    ((id seq3) (desc ()) (seq yyyyyyyyyyyyyyyNNN))
    ((id seq) (desc ("4 > has many '>' in header")) (seq ACTGactg))
    ((id seq) (desc (5)) (seq actG))
    ((id empty) (desc ("seq at end")) (seq ""))

  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.tricky_seqs in
  let _ =
    Fasta.In_channel.with_file name ~f:(fun chan ->
        Fasta.In_channel.iter_records chan ~f:(fun record ->
            print_endline (serialize record) ) )
  in
  [%expect
    {|
    ((id "") (desc ("empty seq at beginning")) (seq ""))
    ((id seq1) (desc ("is fun")) (seq AACTGGNNN))
    ((id seq2) (desc ()) (seq AATCCTGNNN))
    ((id "") (desc ("empty seq 1")) (seq ""))
    ((id "") (desc ("empty seq 2")) (seq ""))
    ((id seq3) (desc ()) (seq yyyyyyyyyyyyyyyNNN))
    ((id seq) (desc ("4 > has many '>' in header")) (seq ACTGactg))
    ((id seq) (desc (5)) (seq actG))
    ((id empty) (desc ("seq at end")) (seq ""))

  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.tricky_seqs in
  let result =
    Fasta.In_channel.with_file_foldi_records name ~init:""
      ~f:(fun i result record ->
        result ^ sprintf "%d -- %s\n" i (serialize record) )
  in
  print_endline result ;
  [%expect
    {|
    0 -- ((id "") (desc ("empty seq at beginning")) (seq ""))
    1 -- ((id seq1) (desc ("is fun")) (seq AACTGGNNN))
    2 -- ((id seq2) (desc ()) (seq AATCCTGNNN))
    3 -- ((id "") (desc ("empty seq 1")) (seq ""))
    4 -- ((id "") (desc ("empty seq 2")) (seq ""))
    5 -- ((id seq3) (desc ()) (seq yyyyyyyyyyyyyyyNNN))
    6 -- ((id seq) (desc ("4 > has many '>' in header")) (seq ACTGactg))
    7 -- ((id seq) (desc (5)) (seq actG))
    8 -- ((id empty) (desc ("seq at end")) (seq ""))

  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.tricky_seqs in
  let result =
    Fasta.In_channel.with_file name ~f:(fun chan ->
        Fasta.In_channel.foldi_records chan ~init:"" ~f:(fun i result record ->
            result ^ sprintf "%d -- %s\n" i (serialize record) ) )
  in
  print_endline result ;
  [%expect
    {|
    0 -- ((id "") (desc ("empty seq at beginning")) (seq ""))
    1 -- ((id seq1) (desc ("is fun")) (seq AACTGGNNN))
    2 -- ((id seq2) (desc ()) (seq AATCCTGNNN))
    3 -- ((id "") (desc ("empty seq 1")) (seq ""))
    4 -- ((id "") (desc ("empty seq 2")) (seq ""))
    5 -- ((id seq3) (desc ()) (seq yyyyyyyyyyyyyyyNNN))
    6 -- ((id seq) (desc ("4 > has many '>' in header")) (seq ACTGactg))
    7 -- ((id seq) (desc (5)) (seq actG))
    8 -- ((id empty) (desc ("seq at end")) (seq ""))

  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.tricky_seqs in
  let _ =
    Fasta.In_channel.with_file_iteri_records name ~f:(fun i record ->
        printf "%d -- %s\n" i (serialize record) )
  in
  [%expect
    {|
    0 -- ((id "") (desc ("empty seq at beginning")) (seq ""))
    1 -- ((id seq1) (desc ("is fun")) (seq AACTGGNNN))
    2 -- ((id seq2) (desc ()) (seq AATCCTGNNN))
    3 -- ((id "") (desc ("empty seq 1")) (seq ""))
    4 -- ((id "") (desc ("empty seq 2")) (seq ""))
    5 -- ((id seq3) (desc ()) (seq yyyyyyyyyyyyyyyNNN))
    6 -- ((id seq) (desc ("4 > has many '>' in header")) (seq ACTGactg))
    7 -- ((id seq) (desc (5)) (seq actG))
    8 -- ((id empty) (desc ("seq at end")) (seq ""))

  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.tricky_seqs in
  let _ =
    Fasta.In_channel.with_file name ~f:(fun chan ->
        Fasta.In_channel.iteri_records chan ~f:(fun i record ->
            printf "%d -- %s\n" i (serialize record) ) )
  in
  [%expect
    {|
    0 -- ((id "") (desc ("empty seq at beginning")) (seq ""))
    1 -- ((id seq1) (desc ("is fun")) (seq AACTGGNNN))
    2 -- ((id seq2) (desc ()) (seq AATCCTGNNN))
    3 -- ((id "") (desc ("empty seq 1")) (seq ""))
    4 -- ((id "") (desc ("empty seq 2")) (seq ""))
    5 -- ((id seq3) (desc ()) (seq yyyyyyyyyyyyyyyNNN))
    6 -- ((id seq) (desc ("4 > has many '>' in header")) (seq ACTGactg))
    7 -- ((id seq) (desc (5)) (seq actG))
    8 -- ((id empty) (desc ("seq at end")) (seq ""))

  |}]

(* Bad inputs *)

let print_record_list lst =
  print_endline
    (Sexp.to_string_hum ~indent:1
       ([%sexp_of: Fasta.Record.t list Or_error.t] lst) )

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.bad_file in
  let actual =
    Or_error.try_with (fun () -> Fasta.In_channel.with_file_records name)
  in
  print_endline
  @@ Sexp.to_string_hum ~indent:1
       ([%sexp_of: Fasta.Record.t List.t Or_error.t] actual) ;
  [%expect
    {|
    (Error (Failure "Not at a header line, but not currently in a sequence"))

  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.bad_file in
  let actual =
    Or_error.try_with (fun () ->
        Fasta.In_channel.with_file_fold_records name ~init:""
          ~f:(fun _acc record -> Fasta.Record.to_string record ) )
  in
  print_endline
  @@ Sexp.to_string_hum ~indent:1 ([%sexp_of: String.t Or_error.t] actual) ;
  [%expect
    {|
    (Error (Failure "Not at a header line, but not currently in a sequence"))


  |}]

(* Sequences *)

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.seqs in
  let () =
    Fasta.In_channel.with_file name ~f:(fun chan ->
        Fasta.In_channel.record_sequence chan
        (* Add sequence index to record description *)
        |> Sequence.mapi ~f:(fun i record ->
               let new_desc =
                 match Fasta.Record.desc record with
                 | None ->
                     Some (sprintf "sequence %d" i)
                 | Some old_desc ->
                     Some (sprintf "%s -- sequence %d" old_desc i)
               in
               Fasta.Record.with_desc new_desc record )
        (* Convert all sequence chars to lowercase *)
        |> Sequence.map ~f:(fun record ->
               let new_seq = String.lowercase (Fasta.Record.seq record) in
               Fasta.Record.with_seq new_seq record )
        (* Print sequences *)
        |> Sequence.iter ~f:(fun record -> print_endline @@ serialize record) )
  in
  [%expect
    {|
    ((id s1) (desc ("apple -- sequence 0")) (seq actgn))
    ((id s2) (desc ("pie -- sequence 1")) (seq actgn))

  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.seqs in
  let () =
    Fasta.In_channel.with_file name ~f:(fun chan ->
        Fasta.In_channel.record_sequence chan
        (* Add sequence index to record description *)
        |> Sequence.mapi ~f:(fun i record ->
               let record = record in
               let new_desc =
                 match Fasta.Record.desc record with
                 | None ->
                     Some (sprintf "sequence %d" i)
                 | Some old_desc ->
                     Some (sprintf "%s -- sequence %d" old_desc i)
               in
               Fasta.Record.with_desc new_desc record )
        (* Convert all sequence chars to lowercase *)
        |> Sequence.map ~f:(fun record ->
               let new_seq = String.lowercase (Fasta.Record.seq record) in
               Fasta.Record.with_seq new_seq record )
        (* Print sequences *)
        |> Sequence.iter ~f:(fun record -> print_endline @@ serialize record) )
  in
  [%expect
    {|
    ((id s1) (desc ("apple -- sequence 0")) (seq actgn))
    ((id s2) (desc ("pie -- sequence 1")) (seq actgn))

  |}]

(* Easiest is probably to use the non exception version to wrap the exception
   version. *)
let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.seqs in
  let result =
    Or_error.try_with (fun () ->
        Fasta.In_channel.with_file name ~f:(fun chan ->
            Fasta.In_channel.record_sequence chan
            (* Add sequence index to record description *)
            |> Sequence.mapi ~f:(fun i record ->
                   if i = 1 then raise_notrace Exit ;
                   let new_desc =
                     match Fasta.Record.desc record with
                     | None ->
                         Some (sprintf "sequence %d" i)
                     | Some old_desc ->
                         Some (sprintf "%s -- sequence %d" old_desc i)
                   in
                   Fasta.Record.with_desc new_desc record )
            (* Convert all sequence chars to lowercase *)
            |> Sequence.map ~f:(fun record ->
                   let new_seq = String.lowercase (Fasta.Record.seq record) in
                   Fasta.Record.with_seq new_seq record )
            (* Print sequences *)
            |> Sequence.iter ~f:(fun record ->
                   print_endline @@ serialize record ) ) )
  in
  print_endline
  @@ Sexp.to_string_hum ~indent:1 ([%sexp_of: unit Or_error.t] result) ;
  [%expect
    {|
((id s1) (desc ("apple -- sequence 0")) (seq actgn))
(Error (Test_bio_io.Test_fasta_in_channel.Exit))
  |}]

(* Simpler version of that. *)
let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.seqs in
  let total_length =
    Or_error.try_with (fun () ->
        Fasta.In_channel.with_file name ~f:(fun chan ->
            Fasta.In_channel.record_sequence chan
            (* Blow up pipeline on second sequence. *)
            |> Sequence.mapi ~f:(fun i record ->
                   if i = 1 then raise_notrace Exit ;
                   record )
            (* Print sequences *)
            |> Sequence.fold ~init:0 ~f:(fun length record ->
                   length + String.length (Fasta.Record.seq record) ) ) )
  in
  print_endline
  @@ Sexp.to_string_hum ~indent:1 ([%sexp_of: int Or_error.t] total_length) ;
  [%expect {|
    (Error (Test_bio_io.Test_fasta_in_channel.Exit))

  |}]

(* Notice how this error is a little weird. *)
let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.bad_file in
  let actual =
    Or_error.try_with (fun () ->
        Fasta.In_channel.with_file name ~f:(fun chan ->
            Fasta.In_channel.record_sequence chan
            |> Sequence.iter ~f:(fun record ->
                   print_endline @@ serialize record ) ) )
  in
  print_endline
  @@ Sexp.to_string_hum ~indent:1 ([%sexp_of: unit Or_error.t] actual) ;
  [%expect
    {|
    (Error (Failure "Not at a header line, but not currently in a sequence"))

  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.bad_file in
  let actual =
    Or_error.try_with (fun () ->
        Fasta.In_channel.with_file name ~f:(fun chan ->
            Fasta.In_channel.record_sequence chan
            |> Sequence.iter ~f:(fun record ->
                   print_endline @@ serialize @@ record ) ) )
  in
  print_endline
  @@ Sexp.to_string_hum ~indent:1 ([%sexp_of: unit Or_error.t] actual) ;
  [%expect
    {|
    (Error (Failure "Not at a header line, but not currently in a sequence"))

  |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.all_carets in
  let () =
    Fasta.In_channel.with_file_iteri_records name ~f:(fun i record ->
        print_endline @@ sprintf "%d -- %s" i (serialize record) )
  in
  [%expect
    {|
    0 -- ((id "") (desc ()) (seq ""))
    1 -- ((id >) (desc ()) (seq ""))
    2 -- ((id >>) (desc ()) (seq ""))
    3 -- ((id "") (desc ()) (seq ""))

  |}]

(* Here are some "normal" usage tests to make sure everyting is looking good.
   Some of these are from the docs.*)

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.seqs in
  let records = Fasta.In_channel.with_file_records name in
  print_endline (Sexp.to_string_hum ([%sexp_of: Fasta.Record.t List.t] records)) ;
  [%expect
    {| (((id s1) (desc (apple)) (seq ACTGn)) ((id s2) (desc (pie)) (seq actgn))) |}]

let%expect_test _ =
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.seqs in
  let total_length =
    Fasta.In_channel.with_file_fold_records name ~init:0
      ~f:(fun length record ->
        length + String.length (Fasta.Record.seq record) )
  in
  printf "%d\n" total_length ; [%expect {| 10 |}]

(* Weird blank lines. *)
let%expect_test _ =
  let name, _chan =
    write_tmp_file Test_fasta_in_channel_data.weird_blank_lines
  in
  let records =
    Or_error.try_with (fun () -> Fasta.In_channel.with_file_records name)
  in
  print_endline
    (Sexp.to_string_hum ([%sexp_of: Fasta.Record.t List.t Or_error.t] records)) ;
  [%expect
    {|
    (Error (Failure "Not at a header line, but not currently in a sequence")) |}]

(* Empty header lines. *)
let%expect_test _ =
  let name, _chan =
    write_tmp_file Test_fasta_in_channel_data.empty_header_lines
  in
  let records = Fasta.In_channel.with_file_records name in
  print_endline (Sexp.to_string_hum ([%sexp_of: Fasta.Record.t List.t] records)) ;
  [%expect
    {| (((id "") (desc ()) (seq ACTG)) ((id "") (desc ()) (seq actg))) |}]

let%expect_test _ =
  let open Fasta in
  let name, _chan = write_tmp_file Test_fasta_in_channel_data.empty in
  let l = Or_error.try_with (fun () -> In_channel.with_file_records name) in
  print_endline @@ Sexp.to_string_hum
  @@ [%sexp_of: Fasta.Record.t List.t Or_error.t] l ;
  [%expect {|
    (Ok ()) |}]
