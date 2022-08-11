open! Base
open Bio_io
module R = Fastq.Record

let print_endline = Stdio.print_endline
let printf = Stdio.printf
let eprintf = Stdio.eprintf
let sprintf = Printf.sprintf
let exit = Caml.exit
let raise_notrace = Caml.raise_notrace
let print_sexp s = print_endline @@ Sexp.to_string_hum ~indent:1 s
let os_equal = Option.equal String.equal

let%expect_test _ =
  Fastq.Record.create ~id:"" ~desc:None ~seq:"" ~qual:"" ~extra:None
  |> Fastq.Record.sexp_of_t |> print_sexp;
  [%expect {| ((id "") (desc ()) (seq "") (qual "") (extra ())) |}]

let%expect_test _ =
  Fastq.Record.create ~id:"a" ~desc:None ~seq:"" ~qual:"" ~extra:None
  |> Fastq.Record.sexp_of_t |> print_sexp;
  [%expect {| ((id a) (desc ()) (seq "") (qual "") (extra ())) |}]

let%expect_test _ =
  Fastq.Record.create ~id:"a" ~desc:(Some "b") ~seq:"" ~qual:"" ~extra:None
  |> Fastq.Record.sexp_of_t |> print_sexp;
  [%expect {| ((id a) (desc (b)) (seq "") (qual "") (extra ())) |}]

let%expect_test _ =
  Fastq.Record.create ~id:"a" ~desc:None ~seq:"actg" ~qual:"" ~extra:None
  |> Fastq.Record.sexp_of_t |> print_sexp;
  [%expect {| ((id a) (desc ()) (seq actg) (qual "") (extra ())) |}]

let%expect_test _ =
  Fastq.Record.create ~id:"a r" ~desc:(Some "b") ~seq:"actg" ~qual:""
    ~extra:None
  |> Fastq.Record.sexp_of_t |> print_sexp;
  [%expect {| ((id "a r") (desc (b)) (seq actg) (qual "") (extra ())) |}]

let%test _ =
  Fastq.Record.equal
    (Fastq.Record.create ~id:"apple" ~desc:None ~seq:"A" ~qual:"!" ~extra:None)
    (Fastq.Record.create ~id:"apple" ~desc:None ~seq:"A" ~qual:"!" ~extra:None)

let%test _ =
  not
    (Fastq.Record.equal
       (Fastq.Record.create ~id:"apple" ~desc:None ~seq:"A" ~qual:"!"
          ~extra:None)
       (Fastq.Record.create ~id:"apple" ~desc:None ~seq:"A" ~qual:"!"
          ~extra:(Some "aaaaa")))

let%test _ =
  not
    (Fastq.Record.equal
       (Fastq.Record.create ~id:"apple" ~desc:None ~seq:"A" ~qual:"!"
          ~extra:None)
       (Fastq.Record.create ~id:"apple" ~desc:None ~seq:"A" ~qual:"?"
          ~extra:None))

let%test _ =
  not
    (Fastq.Record.equal
       (Fastq.Record.create ~id:"apple" ~desc:None ~seq:"A" ~qual:"!"
          ~extra:None)
       (Fastq.Record.create ~id:"apple" ~desc:None ~seq:"B" ~qual:"!"
          ~extra:None))

let%test _ =
  not
    (Fastq.Record.equal
       (Fastq.Record.create ~id:"apple" ~desc:None ~seq:"A" ~qual:"!"
          ~extra:None)
       (Fastq.Record.create ~id:"apple" ~desc:(Some "") ~seq:"A" ~qual:"!"
          ~extra:None))

let%test _ =
  not
    (Fastq.Record.equal
       (Fastq.Record.create ~id:"apple" ~desc:None ~seq:"A" ~qual:"!"
          ~extra:None)
       (Fastq.Record.create ~id:"applE" ~desc:None ~seq:"A" ~qual:"!"
          ~extra:None))

(* Sequence length *)
let%test _ =
  let r =
    Fastq.Record.create ~id:"apple" ~desc:None ~seq:"" ~qual:"" ~extra:None
  in
  Int.(0 = Fastq.Record.seq_length r)

let%test _ =
  let r =
    Fastq.Record.create ~id:"apple" ~desc:None ~seq:"a" ~qual:"!" ~extra:None
  in
  Int.(1 = Fastq.Record.seq_length r)

let%test _ =
  let r =
    Fastq.Record.create ~id:"apple" ~desc:None ~seq:"aa" ~qual:"!!" ~extra:None
  in
  Int.(2 = Fastq.Record.seq_length r)

let%test "if spaces are in the sequence, they are counted as part of the length"
    =
  let r =
    Fastq.Record.create ~id:"apple" ~desc:None ~seq:"a a" ~qual:"! !"
      ~extra:None
  in
  Int.(3 = Fastq.Record.seq_length r)

let%test_unit "nothing bad happens if seq and qual are different lengths" =
  ignore
  @@ Fastq.Record.create ~id:"" ~desc:None ~seq:"a" ~qual:"aa" ~extra:None

let%expect_test "basic field access" =
  let open Fastq.Record in
  let r =
    create ~id:"apple" ~desc:(Some "pie") ~seq:"actgN" ~qual:"....?"
      ~extra:(Some "hehe")
  in
  (* Value exn needs parens for earlier OCaml versions. *)
  print_endline @@ id r;
  print_endline @@ Option.value_exn (desc r);
  print_endline @@ seq r;
  print_endline @@ qual r;
  print_endline @@ Option.value_exn (extra r);
  [%expect {|
    apple
    pie
    actgN
    ....?
    hehe |}]

let%expect_test "basic field access" =
  let open Fastq.Record in
  let r =
    create ~id:"apple" ~desc:(Some "pie") ~seq:"actgN" ~qual:"....?"
      ~extra:(Some "hehe")
  in
  let r2 =
    r |> with_id "a" |> with_seq "b" |> with_desc None |> with_qual "."
    |> with_extra None
  in
  r2 |> sexp_of_t |> Stdio.print_s;
  [%expect {| ((id a) (desc ()) (seq b) (qual .) (extra ())) |}];
  r2 |> to_string |> print_endline;
  [%expect {|
    @a
    b
    +
    . |}]

let%test_unit "reverse complement" =
  let seq = "ATUGCYRSWKMBDHVNatugcyrswkmbdhvn!@#$%" in
  let comp = "TAACGRYSWMKVHDBNtaacgryswmkvhdbn!@#$%" in
  let rev_seq = String.rev seq in
  let qual = "1234123412341234123412341234123412341" in
  let rev_qual = String.rev qual in
  let rev_comp = "%$#@!nbdhvkmwsyrgcaatNBDHVKMWSYRGCAAT" in
  let r = R.create ~seq ~qual ~id:"" ~desc:None ~extra:None in
  let r_comp = R.comp r in
  let r_rev = R.rev r in
  let r_rev_comp = R.rev_comp r in

  assert (String.(comp = R.seq r_comp));
  assert (String.(qual = R.qual r_comp));

  assert (String.(rev_seq = R.seq r_rev));
  assert (String.(rev_qual = R.qual r_rev));

  assert (String.(rev_comp = R.seq r_rev_comp));
  assert (String.(rev_qual = R.qual r_rev_comp))
