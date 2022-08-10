open! Base
open Bio_io

let print_endline = Stdio.print_endline
let printf = Stdio.printf
let eprintf = Stdio.eprintf
let sprintf = Printf.sprintf
let exit = Caml.exit
let raise_notrace = Caml.raise_notrace
let print_sexp s = print_endline @@ Sexp.to_string_hum ~indent:1 s
let os_equal = Option.equal String.equal

let%expect_test _ =
  Fasta.Record.create ~id:"" ~desc:None ~seq:""
  |> Fasta.Record.sexp_of_t |> print_sexp;
  [%expect {| ((id "") (desc ()) (seq "")) |}]

let%expect_test _ =
  Fasta.Record.create ~id:"a" ~desc:None ~seq:""
  |> Fasta.Record.sexp_of_t |> print_sexp;
  [%expect {| ((id a) (desc ()) (seq "")) |}]

let%expect_test _ =
  Fasta.Record.create ~id:"a" ~desc:(Some "b") ~seq:""
  |> Fasta.Record.sexp_of_t |> print_sexp;
  [%expect {| ((id a) (desc (b)) (seq "")) |}]

let%expect_test _ =
  Fasta.Record.create ~id:"a" ~desc:None ~seq:"actg"
  |> Fasta.Record.sexp_of_t |> print_sexp;
  [%expect {| ((id a) (desc ()) (seq actg)) |}]

let%expect_test _ =
  Fasta.Record.create ~id:"a r" ~desc:(Some "b") ~seq:"actg"
  |> Fasta.Record.sexp_of_t |> print_sexp;
  [%expect {| ((id "a r") (desc (b)) (seq actg)) |}]

(* let%test _ = String.equal (Fasta.Record.of_header_exn ">apple" |>
   Fasta.Record.id) "apple"

   let%test _ = Fasta.Record.equal (Or_error.ok_exn @@ Fasta.Record.of_header
   ">") (Fasta.Record.create ~id:"" ~desc:None ~seq:"")

   let%test _ = Fasta.Record.equal (Or_error.ok_exn @@ Fasta.Record.of_header
   ">apple") (Fasta.Record.create ~id:"apple" ~desc:None ~seq:"")

   let%test _ = Fasta.Record.equal (Fasta.Record.of_header_exn ">apple" |>
   Fasta.Record.with_desc None) (Fasta.Record.create ~id:"apple" ~desc:None
   ~seq:"")

   let%test _ = Fasta.Record.equal (Fasta.Record.of_header_exn ">apple" |>
   Fasta.Record.with_seq "A" |> Fasta.Record.with_desc (Some "B"))
   (Fasta.Record.create ~id:"apple" ~desc:(Some "B") ~seq:"A")

   let%test _ = Fasta.Record.equal (Or_error.ok_exn @@ Fasta.Record.of_header
   ">apple pie") (Fasta.Record.create ~id:"apple" ~desc:(Some "pie") ~seq:"")

   let%test _ = Fasta.Record.equal (Fasta.Record.of_header_exn ">APPLE pie")
   (Fasta.Record.of_header_exn ">apple pie" |> Fasta.Record.with_id "APPLE")

   let%test _ = not ((Or_error.equal Fasta.Record.equal) (Fasta.Record.of_header
   ">apple pie") (Fasta.Record.of_header ""))

   let%test _ = not ((Or_error.equal Fasta.Record.equal) (Fasta.Record.of_header
   ">apple pie") (Fasta.Record.of_header ">"))

   let%test _ = not ((Or_error.equal Fasta.Record.equal) (Fasta.Record.of_header
   ">apple pie") (Fasta.Record.of_header ">apple")) *)

(* Sequence length *)
let%test _ =
  let r = Fasta.Record.create ~id:"apple" ~desc:None ~seq:"" in
  Int.(0 = Fasta.Record.seq_length r)

let%test _ =
  let r = Fasta.Record.create ~id:"apple" ~desc:None ~seq:"a" in
  Int.(1 = Fasta.Record.seq_length r)

let%test _ =
  let r = Fasta.Record.create ~id:"apple" ~desc:None ~seq:"aa" in
  Int.(2 = Fasta.Record.seq_length r)

let%test "if spaces are in the sequence, they are counted as part of the length"
    =
  let r = Fasta.Record.create ~id:"apple" ~desc:None ~seq:"a a" in
  Int.(3 = Fasta.Record.seq_length r)
