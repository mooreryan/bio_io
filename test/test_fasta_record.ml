open! Base
open Bio_io
module R = Fasta.Record

let print_endline = Stdio.print_endline
let printf = Stdio.printf
let eprintf = Stdio.eprintf
let sprintf = Printf.sprintf
let exit = Caml.exit
let raise_notrace = Caml.raise_notrace
let print_sexp s = print_endline @@ Sexp.to_string_hum ~indent:1 s
let os_equal = Option.equal String.equal

let%expect_test _ =
  R.create ~id:"" ~desc:None ~seq:"" |> R.sexp_of_t |> print_sexp;
  [%expect {| ((id "") (desc ()) (seq "")) |}]

let%expect_test _ =
  R.create ~id:"a" ~desc:None ~seq:"" |> R.sexp_of_t |> print_sexp;
  [%expect {| ((id a) (desc ()) (seq "")) |}]

let%expect_test _ =
  R.create ~id:"a" ~desc:(Some "b") ~seq:"" |> R.sexp_of_t |> print_sexp;
  [%expect {| ((id a) (desc (b)) (seq "")) |}]

let%expect_test _ =
  R.create ~id:"a" ~desc:None ~seq:"actg" |> R.sexp_of_t |> print_sexp;
  [%expect {| ((id a) (desc ()) (seq actg)) |}]

let%expect_test _ =
  R.create ~id:"a r" ~desc:(Some "b") ~seq:"actg" |> R.sexp_of_t |> print_sexp;
  [%expect {| ((id "a r") (desc (b)) (seq actg)) |}]

let%test_unit _ =
  let r1 = R.create ~id:"apple" ~desc:None ~seq:"" in
  let r2 = r1 |> R.with_id "thing" in
  assert (String.("thing" = R.id r2));
  assert (R.(r1 = with_id "apple" r2))

(* let%test _ = String.equal (R.of_header_exn ">apple" |> R.id) "apple"

   let%test _ = R.equal (Or_error.ok_exn @@ R.of_header ">") (R.create ~id:""
   ~desc:None ~seq:"")

   let%test _ = R.equal (Or_error.ok_exn @@ R.of_header ">apple") (R.create
   ~id:"apple" ~desc:None ~seq:"")

   let%test _ = R.equal (R.of_header_exn ">apple" |> R.with_desc None) (R.create
   ~id:"apple" ~desc:None ~seq:"")

   let%test _ = R.equal (R.of_header_exn ">apple" |> R.with_seq "A" |>
   R.with_desc (Some "B")) (R.create ~id:"apple" ~desc:(Some "B") ~seq:"A")

   let%test _ = R.equal (Or_error.ok_exn @@ R.of_header ">apple pie") (R.create
   ~id:"apple" ~desc:(Some "pie") ~seq:"")

   let%test _ = R.equal (R.of_header_exn ">APPLE pie") (R.of_header_exn ">apple
   pie" |> R.with_id "APPLE")

   let%test _ = not ((Or_error.equal R.equal) (R.of_header ">apple pie")
   (R.of_header ""))

   let%test _ = not ((Or_error.equal R.equal) (R.of_header ">apple pie")
   (R.of_header ">"))

   let%test _ = not ((Or_error.equal R.equal) (R.of_header ">apple pie")
   (R.of_header ">apple")) *)

(* Sequence length *)
let%test _ =
  let r = R.create ~id:"apple" ~desc:None ~seq:"" in
  Int.(0 = R.seq_length r)

let%test _ =
  let r = R.create ~id:"apple" ~desc:None ~seq:"a" in
  Int.(1 = R.seq_length r)

let%test _ =
  let r = R.create ~id:"apple" ~desc:None ~seq:"aa" in
  Int.(2 = R.seq_length r)

let%test "if spaces are in the sequence, they are counted as part of the length"
    =
  let r = R.create ~id:"apple" ~desc:None ~seq:"a a" in
  Int.(3 = R.seq_length r)

let%test_unit "reverse complement" =
  let seq = "ATUGCYRSWKMBDHVNatugcyrswkmbdhvn!@#$%" in
  let comp = "TAACGRYSWMKVHDBNtaacgryswmkvhdbn!@#$%" in
  let rev_seq = String.rev seq in
  let rev_comp = "%$#@!nbdhvkmwsyrgcaatNBDHVKMWSYRGCAAT" in
  let r = R.create ~seq ~id:"" ~desc:None in
  let r_comp = R.comp r in
  let r_rev = R.rev r in
  let r_rev_comp = R.rev_comp r in

  assert (String.(comp = R.seq r_comp));
  assert (String.(rev_seq = R.seq r_rev));
  assert (String.(rev_comp = R.seq r_rev_comp))
