open! Core_kernel
open Bio_io

module Q = Quickcheck

let print_cigar_parse_result x =
  let redact s =
    Re2.replace_exn (Re2.create_exn "\\(.*Exn") s ~f:(fun _ -> "(REDACTED Exn")
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
  Cigar.num_matches cigar = 11

let%test _ =
  let cigar = Cigar.of_string "1M1D1I1M" |> Or_error.ok_exn in
  Cigar.num_matches cigar = 2

let%test _ =
  let cigar = Cigar.of_string "DIDIDIDIDIDIDDDI" |> Or_error.ok_exn in
  Cigar.num_matches cigar = 0

let%test_unit _ =
  [%test_result: int]
    (Cigar.alignment_length @@ Cigar.of_string_exn "1M2I3D5M")
    ~expect:11
let%test_unit _ =
  [%test_result: int]
    (Cigar.num_gaps @@ Cigar.of_string_exn "1M2I3D5M")
    ~expect:5
let%test_unit _ =
  [%test_result: int]
    (Cigar.num_matches @@ Cigar.of_string_exn "1M2I3D5M")
    ~expect:6
let%test_unit _ =
  [%test_result: int]
    (Cigar.query_length @@ Cigar.of_string_exn "1M2I3D5M")
    ~expect:8
let%test_unit _ =
  [%test_result: int]
    (Cigar.target_length @@ Cigar.of_string_exn "1M2I3D5M")
    ~expect:9

(* Property tests *)

(* Basic generators. *)

(* Generates non-negative ints of any size, unlike
   Q.Generator.small_non_negative_int. *)
let positive_int_generator =
  Q.Generator.filter Int.quickcheck_generator ~f:(fun n -> n > 0)

(* Counts are technically optional. If there is no count, it is treated as a
   count of one. Will generate count of zero. *)
let count_generator = Option.quickcheck_generator positive_int_generator

let num_chunks_generator = Q.Generator.small_non_negative_int
let operation_generator = Q.Generator.of_list [ "M"; "D"; "I" ]
let operation_list_generator = List.gen_non_empty operation_generator

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
  List.gen_with_length num_chunks cigar_chunk_generator >>= fun op_list ->
  Q.Generator.return @@ String.concat op_list ~sep:""

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

let%test_unit "or_error returning parser never raises" =
  Q.test String.quickcheck_generator ~shrinker:String.quickcheck_shrinker
    ~sexp_of:String.sexp_of_t ~f:(fun s ->
      let (_ : Cigar.t Or_error.t) = Cigar.of_string s in
      ())

let%test_unit "_exn cigar parsing with valid inputs doesn't raise" =
  let generator = cigar_string_generator cigar_chunk_maybe_count_generator in
  Q.test generator ~sexp_of:String.sexp_of_t ~f:(fun s ->
      let (_ : Cigar.t) = Cigar.of_string_exn s in
      ())

let%test_unit "round tripping cigar string with counts parsing" =
  let generator = cigar_string_generator cigar_chunk_with_count_generator in
  Q.test generator ~sexp_of:String.sexp_of_t ~f:(fun cigar ->
      let cigar' = cigar |> Cigar.of_string_exn |> Cigar.to_string in
      [%test_eq: string] cigar cigar')

let%test_unit "cigar equals works" =
  let generator = cigar_string_generator cigar_chunk_maybe_count_generator in
  Q.test generator ~sexp_of:String.sexp_of_t ~f:(fun s ->
      let c1 = Cigar.of_string_exn s in
      let c2 = Cigar.of_string_exn s in
      assert (Cigar.equal c1 c2))

let%test_unit "count of one and blank are the same" =
  Q.test single_operation_string_tuple_generator
    ~sexp_of:(Tuple2.sexp_of_t String.sexp_of_t String.sexp_of_t)
    ~f:(fun (with_blanks, with_ones) ->
      let x = Cigar.sexp_of_t @@ Cigar.of_string_exn with_ones in
      let y = Cigar.sexp_of_t @@ Cigar.of_string_exn with_blanks in
      [%test_eq: Sexp.t] x y)

(* Hacky regex implementations of length-like functions. Use it to check the
   real implementation! *)

let count_implementation re cigar =
  Re2.get_matches_exn re cigar
  |> List.map ~f:(fun m -> m |> Re2.Match.get_exn ~sub:(`Index 1))
  |> List.fold ~init:0 ~f:(fun count -> function
       (* Remeber that blank string for count is really 1. *)
       | "" -> count + 1
       | s -> count + Int.of_string s)

let alignment_length cigar =
  count_implementation (Re2.create_exn "([0-9]*)[MDI]") cigar

let num_gaps cigar = count_implementation (Re2.create_exn "([0-9]*)[ID]") cigar

let num_matches cigar = count_implementation (Re2.create_exn "([0-9]*)M") cigar

let query_length cigar =
  count_implementation (Re2.create_exn "([0-9]*)[IM]") cigar

let target_length cigar =
  count_implementation (Re2.create_exn "([0-9]*)[DM]") cigar

let count_fun_tester reference_impl real_impl =
  let generator = cigar_string_generator cigar_chunk_maybe_count_generator in
  Q.test generator ~sexp_of:String.sexp_of_t ~f:(fun cigar_s ->
      let x = reference_impl cigar_s in
      let cigar = Cigar.of_string_exn cigar_s in
      [%test_eq: int] x (real_impl cigar))

let%test_unit _ = count_fun_tester alignment_length Cigar.alignment_length
let%test_unit _ = count_fun_tester num_gaps Cigar.num_gaps
let%test_unit _ = count_fun_tester num_matches Cigar.num_matches
let%test_unit _ = count_fun_tester query_length Cigar.query_length
let%test_unit _ = count_fun_tester target_length Cigar.target_length