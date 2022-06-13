open! Base
open Bio_io
module Q = Base_quickcheck
module QG = Base_quickcheck.Generator

module Tuple2 = struct
  type ('a, 'b) t = 'a * 'b [@@deriving sexp]
end

let print_endline = Stdio.print_endline

let print_cigar_parse_result x =
  let redact s =
    let re = Re.Perl.compile_pat "\\(.*Exn" in
    Re.replace_string re ~by:"(REDACTED Exn" s
  in
  print_endline @@ redact
  @@ Sexp.to_string_hum ~indent:1 ([%sexp_of: Cigar.t Or_error.t] x)

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "Apple";
  [%expect
    {|
    (Error
     ("Error parsing cigar string"
      (REDACTED Exn "Expected int or operation. Got A"))) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "123Aapple";
  [%expect
    {|
    (Error
     ("Error parsing cigar string"
      (REDACTED Exn "Expected int or operation. Got A"))) |}]

let%expect_test "negative count in the middle" =
  print_cigar_parse_result @@ Cigar.of_string "12M-3I";
  [%expect
    {|
    (Error
     ("Error parsing cigar string"
      (REDACTED Exn "Expected int or operation. Got -"))) |}]

let%expect_test "negative count at the start" =
  print_cigar_parse_result @@ Cigar.of_string "-12M3I";
  [%expect
    {|
    (Error
     ("Error parsing cigar string"
      (REDACTED Exn "Expected int or operation. Got -"))) |}]
(* Empty string gives error. *)

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "";
  [%expect {| (Ok ()) |}]

(* Zero count chunks give errors. *)

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "0M";
  [%expect
    {| (Error ("Error parsing cigar string" "Length must be > 0.  Got 0.")) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "0D";
  [%expect
    {| (Error ("Error parsing cigar string" "Length must be > 0.  Got 0.")) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "0I";
  [%expect
    {| (Error ("Error parsing cigar string" "Length must be > 0.  Got 0.")) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "M";
  [%expect {| (Ok ((1 Match))) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "D";
  [%expect {| (Ok ((1 Deletion))) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "I";
  [%expect {| (Ok ((1 Insertion))) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "1M";
  [%expect {| (Ok ((1 Match))) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "1D";
  [%expect {| (Ok ((1 Deletion))) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "1I";
  [%expect {| (Ok ((1 Insertion))) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "2M";
  [%expect {| (Ok ((2 Match))) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "2D";
  [%expect {| (Ok ((2 Deletion))) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "2I";
  [%expect {| (Ok ((2 Insertion))) |}]

let%expect_test _ =
  print_cigar_parse_result @@ Cigar.of_string "1M2D3I4M5D6I";
  [%expect
    {|
    (Ok
     ((1 Match) (2 Deletion) (3 Insertion) (4 Match) (5 Deletion) (6 Insertion))) |}]

let%expect_test "for now, same op back to back is okay" =
  print_cigar_parse_result @@ Cigar.of_string "2M3M";
  [%expect {| (Ok ((2 Match) (3 Match))) |}]

let%expect_test "no integers mean 1" =
  print_cigar_parse_result @@ Cigar.of_string "M1MDI";
  [%expect {|
    (Ok ((1 Match) (1 Match) (1 Deletion) (1 Insertion))) |}]

let%expect_test "no integers mean 1" =
  print_endline @@ Cigar.to_string @@ Cigar.of_string_exn "MDIM";
  [%expect {| 1M1D1I1M |}]

(* Lengths and numbers of things. *)

let%test _ =
  let cigar = Cigar.of_string "10MM" |> Or_error.ok_exn in
  Or_error.ok_exn @@ Cigar.num_matches cigar = 11

let%test _ =
  let cigar = Cigar.of_string "1M1D1I1M" |> Or_error.ok_exn in
  Or_error.ok_exn @@ Cigar.num_matches cigar = 2

let%test _ =
  let cigar = Cigar.of_string "DIDIDIDIDIDIDDDI" |> Or_error.ok_exn in
  Or_error.ok_exn @@ Cigar.num_matches cigar = 0

let%test_unit _ =
  [%test_result: int]
    (Cigar.alignment_length_exn @@ Cigar.of_string_exn "1M2I3D5M")
    ~expect:11

let%test_unit _ =
  [%test_result: int]
    (Cigar.num_gaps_exn @@ Cigar.of_string_exn "1M2I3D5M")
    ~expect:5

let%test_unit _ =
  [%test_result: int]
    (Cigar.num_matches_exn @@ Cigar.of_string_exn "1M2I3D5M")
    ~expect:6

let%test_unit _ =
  [%test_result: int]
    (Cigar.query_length_exn @@ Cigar.of_string_exn "1M2I3D5M")
    ~expect:8

let%test_unit _ =
  [%test_result: int]
    (Cigar.target_length_exn @@ Cigar.of_string_exn "1M2I3D5M")
    ~expect:9

(* Alignment drawing functions *)

let drawing_test_input_string = "1M2I3D5M"

let%expect_test _ =
  print_endline @@ Cigar.draw_exn
  @@ Cigar.of_string_exn drawing_test_input_string;
  [%expect {|
    t: X--XXXXXXXX
    q: XXX---XXXXX
    o: MIIDDDMMMMM |}]

let%expect_test _ =
  print_endline
  @@ Cigar.draw_exn ~gap:'.' ~non_gap:'+'
  @@ Cigar.of_string_exn drawing_test_input_string;
  [%expect {|
    t: +..++++++++
    q: +++...+++++
    o: MIIDDDMMMMM |}]

let%expect_test _ =
  print_endline @@ Cigar.draw_exn @@ Cigar.of_string_exn "";
  [%expect {| |}]

let%expect_test _ =
  print_endline @@ Cigar.draw_exn ~wrap:10 @@ Cigar.of_string_exn "25M";
  [%expect
    {|
    t: XXXXXXXXXX
    q: XXXXXXXXXX
    o: MMMMMMMMMM

    t: XXXXXXXXXX
    q: XXXXXXXXXX
    o: MMMMMMMMMM

    t: XXXXX
    q: XXXXX
    o: MMMMM |}]

(* Property tests *)

(* Basic generators. *)

(* Generates non-negative ints of any size, unlike
   Q.Generator.small_non_negative_int.

   TODO do I still need this with base_quickcheck? *)
let positive_int_generator = Q.Generator.filter QG.int ~f:(fun n -> n > 0)

(* Counts are technically optional. If there is no count, it is treated as a
   count of one. Will generate count of zero. *)
let count_generator = QG.option positive_int_generator
let small_count_generator = QG.small_strictly_positive_int
(* TODO make a small count generating cigar generator. *)

let num_chunks_generator = QG.small_positive_or_zero_int
let operation_generator = Q.Generator.of_list [ "M"; "D"; "I" ]
let operation_list_generator = QG.list_non_empty operation_generator

(* Compound generators. *)

(* Generate a Cigar string chunk: "23432I", etc. *)
let cigar_chunk_with_count_generator =
  Q.Generator.map2 operation_generator positive_int_generator
    ~f:(fun op count -> Int.to_string count ^ op)

(* Generate a Cigar string chunk. These may have no count: "D", "1D", "23432I",
   etc. *)
let cigar_chunk_maybe_count_generator =
  Q.Generator.map2 operation_generator count_generator ~f:(fun op -> function
    | Some count -> Int.to_string count ^ op | None -> op)

(* Let's you specify the types of cigar chunks...always counts or maybe
   counts. *)
let cigar_string_generator cigar_chunk_generator =
  let open Q.Generator.Monad_infix in
  num_chunks_generator >>= fun num_chunks ->
  QG.list_with_length ~length:num_chunks cigar_chunk_generator
  >>= fun op_list -> Q.Generator.return @@ String.concat op_list ~sep:""

(* Generate a tuple of cigar string made of only count 1 operations. The first
   elem uses blanks for count of 1. The second uses "1" to denote a count of
   one. They should be equal. *)
let single_operation_string_tuple_generator =
  Q.Generator.map operation_list_generator ~f:(fun op_list ->
      let with_blanks = String.concat op_list ~sep:"" in
      let with_ones =
        List.fold op_list ~init:"" ~f:(fun acc op -> acc ^ "1" ^ op)
      in
      (with_blanks, with_ones))

(* Quickcheck tests *)

let make_string_test quickcheck_generator :
    (module Q.Test.S with type t = string) =
  (module struct
    type t = string [@@deriving sexp]

    let quickcheck_generator = quickcheck_generator
    let quickcheck_shrinker = Q.Shrinker.string
  end)

let make_non_shrinking_string_test quickcheck_generator :
    (module Q.Test.S with type t = string) =
  (module struct
    type t = string [@@deriving sexp]

    let quickcheck_generator = quickcheck_generator
    let quickcheck_shrinker = Q.Shrinker.atomic
  end)

let%test_unit "or_error returning parser never raises" =
  Q.Test.run_exn (make_string_test QG.string) ~f:(fun s ->
      let (_ : Cigar.t Or_error.t) = Cigar.of_string s in
      ())

let%test_unit "_exn cigar parsing with valid inputs doesn't raise" =
  let generator = cigar_string_generator cigar_chunk_maybe_count_generator in
  Q.Test.run_exn (make_non_shrinking_string_test generator) ~f:(fun s ->
      let (_ : Cigar.t) = Cigar.of_string_exn s in
      ())

let%test_unit "round tripping cigar string with counts parsing" =
  let generator = cigar_string_generator cigar_chunk_with_count_generator in
  Q.Test.run_exn (make_non_shrinking_string_test generator) ~f:(fun cigar ->
      let cigar' = cigar |> Cigar.of_string_exn |> Cigar.to_string in
      [%test_eq: string] cigar cigar')

let%test_unit "cigar equals works" =
  let generator = cigar_string_generator cigar_chunk_maybe_count_generator in
  Q.Test.run_exn (make_non_shrinking_string_test generator) ~f:(fun s ->
      let c1 = Cigar.of_string_exn s in
      let c2 = Cigar.of_string_exn s in
      assert (Cigar.equal c1 c2))

let%test_unit "count of one and blank are the same" =
  Q.Test.run_exn
    (module struct
      type t = (string, string) Tuple2.t [@@deriving sexp]

      let quickcheck_generator = single_operation_string_tuple_generator
      let quickcheck_shrinker = Q.Shrinker.atomic
    end)
    ~f:(fun (with_blanks, with_ones) ->
      let x = Cigar.sexp_of_t @@ Cigar.of_string_exn with_ones in
      let y = Cigar.sexp_of_t @@ Cigar.of_string_exn with_blanks in
      [%test_eq: Sexp.t] x y)

(* Length functions don't crash *)

(* let%test_unit _ =
 *   let generator = cigar_string_generator cigar_chunk_maybe_count_generator in
 *   Q.Test.run_exn (make_non_shrinking_string_test generator) ~f:(fun cigar_s ->
 *       let cigar = Cigar.of_string_exn cigar_s in
 *       let _x = Cigar.draw cigar in
 *       ()) *)

(* Hacky regex implementations of length-like functions. Use it to check the
   real implementation! *)
let count_implementation re cigar =
  let add_exn x y =
    let sum = x + y in
    let sign_x = Int.sign x in
    if Sign.(sign_x = Int.sign y && sign_x <> Int.sign sum) then
      raise (Cigar.Int_overflow (x, y))
    else sum
  in
  let f () =
    let n =
      Re.all re cigar
      |> List.map ~f:(fun m -> Re.Group.get m 1)
      (* Folding and adding up ints can overflow...which wraps around in
         OCaml. *)
      |> List.fold ~init:0 ~f:(fun count -> function
           (* Remeber that blank string for count is really 1. *)
           | "" -> add_exn count 1
           | s -> add_exn count (Int.of_string s))
    in
    (* >= because empty strings return length of 0. *)
    assert (n >= 0);
    n
  in
  Utils.try0 f

let re_MID_chunk = Re.Perl.compile_pat "([0-9]*)[MID]"
let re_ID_chunk = Re.Perl.compile_pat "([0-9]*)[ID]"
let re_M_chunk = Re.Perl.compile_pat "([0-9]*)M"
let re_MI_chunk = Re.Perl.compile_pat "([0-9]*)[MI]"
let re_MD_chunk = Re.Perl.compile_pat "([0-9]*)[MD]"
let alignment_length cigar = count_implementation re_MID_chunk cigar
let num_gaps cigar = count_implementation re_ID_chunk cigar
let num_matches cigar = count_implementation re_M_chunk cigar
let query_length cigar = count_implementation re_MI_chunk cigar
let target_length cigar = count_implementation re_MD_chunk cigar

let count_fun_tester reference_impl real_impl =
  let generator = cigar_string_generator cigar_chunk_maybe_count_generator in
  Q.Test.run_exn (make_non_shrinking_string_test generator) ~f:(fun cigar_s ->
      (* This will test that the same Int_overflow exception is thrown as
         well. *)
      let x = reference_impl cigar_s in
      let y = real_impl (Cigar.of_string_exn cigar_s) in
      [%test_eq: int Or_error.t] x y)

let%test_unit _ = count_fun_tester alignment_length Cigar.alignment_length
let%test_unit _ = count_fun_tester num_gaps Cigar.num_gaps
let%test_unit _ = count_fun_tester num_matches Cigar.num_matches
let%test_unit _ = count_fun_tester query_length Cigar.query_length
let%test_unit _ = count_fun_tester target_length Cigar.target_length

(* Drawing functions shouldn't crash on valid inputs. *)

let%test_unit _ =
  let generator = cigar_string_generator cigar_chunk_maybe_count_generator in
  Q.Test.run_exn (make_non_shrinking_string_test generator) ~f:(fun cigar_s ->
      let cigar = Cigar.of_string_exn cigar_s in
      let _x = Cigar.draw cigar in
      ())
