open! Base
open Bio_io
module Q = Base_quickcheck
module QG = Base_quickcheck.Generator
module Ic = Private.Peekable_in_channel

let print_s = Stdio.print_s

let print_endline = Stdio.print_endline

let print_string = Stdio.print_string

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
    | true ->
        Stdlib.Sys.remove fname
    | false ->
        ()
  in
  let chan = Stdio.Out_channel.create fname in
  Stdio.Out_channel.output_string chan data ;
  Stdio.Out_channel.flush chan ;
  Stdio.Out_channel.close chan ;
  (fname, chan)

let write_tmp_file' (data : Bytes.t) =
  let fname =
    Stdlib.Filename.concat
      (Stdlib.Filename.get_temp_dir_name ())
      "bio_io_test_ic.txt"
  in
  let () =
    match Stdlib.Sys.file_exists fname with
    | true ->
        Stdlib.Sys.remove fname
    | false ->
        ()
  in
  let chan = Stdio.Out_channel.create fname in
  Stdio.Out_channel.output_bytes chan data ;
  Stdio.Out_channel.flush chan ;
  Stdio.Out_channel.close chan ;
  (fname, chan)

let print_string_s s = Stdio.print_s @@ String.sexp_of_t s

let%expect_test _ =
  let f ic = Option.value ~default:"NONE" @@ Ic.input_line ic in
  let data =
    String.concat ~sep:"\n"
      [ "0. apple pie"
      ; "1. is  "
      ; "2. really"
      ; "3. good to "
      ; "4. eat"
      ; "5. and"
      ; "6. i"
      ; "7. LIKE"
      ; "8. it" ]
  in
  let fname, _chan = write_tmp_file data in
  let ic = Ic.create fname in
  print_string_s @@ f ic ;
  print_string_s @@ f ic ;
  print_string_s @@ f ic ;
  print_string_s @@ f ic ;
  print_string_s @@ f ic ;
  print_string_s @@ f ic ;
  print_string_s @@ f ic ;
  print_string_s @@ f ic ;
  print_string_s @@ f ic ;
  print_string_s @@ f ic ;
  print_string_s @@ f ic ;
  [%expect
    {|
    "0. apple pie"
    "1. is  "
    "2. really"
    "3. good to "
    "4. eat"
    "5. and"
    "6. i"
    "7. LIKE"
    "8. it"
    NONE
    NONE |}]

let%expect_test _ =
  let input ic = Option.value ~default:"NONE" @@ Ic.input_line ic in
  let peek ic = Option.value ~default:"NONE" @@ Ic.peek_line ic in
  let data =
    String.concat ~sep:"\n"
      [ "0. apple pie"
      ; "1. is  "
      ; "2. really"
      ; "3. good to "
      ; "4. eat"
      ; "5. and"
      ; "6. i"
      ; "7. LIKE"
      ; "8. it" ]
  in
  let fname, _chan = write_tmp_file data in
  let ic = Ic.create fname in
  print_string_s @@ peek ic ;
  print_string_s @@ peek ic ;
  print_string_s @@ input ic ;
  print_string_s @@ peek ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ peek ic ;
  print_string_s @@ peek ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ peek ic ;
  [%expect
    {|
    "0. apple pie"
    "0. apple pie"
    "0. apple pie"
    "1. is  "
    "1. is  "
    "2. really"
    "3. good to "
    "3. good to "
    "3. good to "
    "4. eat"
    "5. and"
    "6. i"
    "7. LIKE"
    "8. it"
    NONE
    NONE
    NONE |}]

let%expect_test "input_line_exn with peek" =
  let input ic = Ic.input_line_exn ic in
  let peek ic = Option.value ~default:"NONE" @@ Ic.peek_line ic in
  let data =
    String.concat ~sep:"\n"
      [ "0. apple pie"
      ; "1. is  "
      ; "2. really"
      ; "3. good to "
      ; "4. eat"
      ; "5. and"
      ; "6. i"
      ; "7. LIKE"
      ; "8. it" ]
  in
  let fname, _chan = write_tmp_file data in
  let ic = Ic.create fname in
  print_string_s @@ peek ic ;
  print_string_s @@ peek ic ;
  print_string_s @@ input ic ;
  print_string_s @@ peek ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ peek ic ;
  print_string_s @@ peek ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  print_string_s @@ input ic ;
  ( try print_string_s @@ input ic
    with End_of_file -> print_endline "End_of_file" ) ;
  ( try print_string_s @@ input ic
    with End_of_file -> print_endline "End_of_file" ) ;
  print_string_s @@ peek ic ;
  [%expect
    {|
    "0. apple pie"
    "0. apple pie"
    "0. apple pie"
    "1. is  "
    "1. is  "
    "2. really"
    "3. good to "
    "3. good to "
    "3. good to "
    "4. eat"
    "5. and"
    "6. i"
    "7. LIKE"
    "8. it"
    End_of_file
    End_of_file
    NONE |}]

let%expect_test _ =
  let f ic = Option.value ~default:"NONE" @@ Ic.input_line ic in
  let data = String.concat ~sep:"\n" ["hi"; ""; "ryan"] in
  let fname, _chan = write_tmp_file data in
  let ic = Ic.create fname in
  print_string_s @@ f ic ;
  print_string_s @@ f ic ;
  print_string_s @@ f ic ;
  [%expect {|
    hi
    ""
    ryan |}]

let%expect_test _ =
  let input ic = Option.value ~default:"NONE" @@ Ic.input_line ic in
  let peek_line ic = Option.value ~default:"NONE" @@ Ic.peek_line ic in
  let peek_char ic =
    Char.to_string @@ Option.value ~default:'?' @@ Ic.peek_char ic
  in
  let data = String.concat ~sep:"\n" ["hi"; ""; "ryan"] in
  let fname, _chan = write_tmp_file data in
  let ic = Ic.create fname in
  print_string_s @@ peek_line ic ;
  print_string_s @@ peek_line ic ;
  print_string_s @@ peek_char ic ;
  print_string_s @@ input ic ;
  print_string_s @@ peek_line ic ;
  print_string_s @@ peek_line ic ;
  print_string_s @@ peek_char ic ;
  print_string_s @@ input ic ;
  print_string_s @@ peek_line ic ;
  print_string_s @@ peek_line ic ;
  print_string_s @@ peek_char ic ;
  print_string_s @@ input ic ;
  print_string_s @@ peek_line ic ;
  print_string_s @@ peek_line ic ;
  print_string_s @@ peek_char ic ;
  print_string_s @@ input ic ;
  [%expect
    {|
    hi
    hi
    h
    hi
    ""
    ""
    "\n"
    ""
    ryan
    ryan
    r
    ryan
    NONE
    NONE
    ?
    NONE |}]

let%expect_test _ =
  let data =
    String.concat ~sep:"\n" ["0. apple pie"; "1. is  "; "2. really"; "3. good"]
  in
  let fname, _chan = write_tmp_file data in
  let print_peek_char ic =
    print_string "char_s: " ;
    print_s @@ [%sexp_of: char option] @@ Ic.peek_char ic
  in
  let print_peek_line ic =
    print_string "line_s: " ;
    print_s @@ [%sexp_of: string option] @@ Ic.peek_line ic
  in
  ignore
    (Ic.with_file fname ~f:(fun ic ->
         print_peek_char ic ;
         print_peek_line ic ;
         let count =
           Ic.fold_lines ic ~init:0 ~f:(fun i line ->
               print_endline (Int.to_string i ^ " => " ^ line) ;
               print_peek_char ic ;
               print_peek_line ic ;
               i + 1 )
         in
         print_endline ("Count: " ^ Int.to_string count) ) ) ;
  [%expect
    {|
    char_s: (0)
    line_s: ("0. apple pie")
    0 => 0. apple pie
    char_s: (1)
    line_s: ("1. is  ")
    1 => 1. is
    char_s: (2)
    line_s: ("2. really")
    2 => 2. really
    char_s: (3)
    line_s: ("3. good")
    3 => 3. good
    char_s: ()
    line_s: ()
    Count: 4 |}]

(* Property tests *)

(* Uses fold_lines internally *)
let%test_unit "input all big file" =
  let data = Bytes.init 1000000 ~f:(fun _ -> Random.char ()) in
  let fname, _ = write_tmp_file' data in
  let expected =
    Stdio.In_channel.with_file fname ~f:Stdio.In_channel.input_all
  in
  let actual = Ic.with_file fname ~f:Ic.input_all in
  [%test_eq: string] actual expected

let%test_unit "input all, peek first" =
  let data = "hey y'all\nwhat's up?" in
  let fname, _ = write_tmp_file data in
  let expected =
    Stdio.In_channel.with_file fname ~f:Stdio.In_channel.input_all
  in
  let actual =
    Ic.with_file fname ~f:(fun ic ->
        let _ = Ic.peek_line ic in
        Ic.input_all ic )
  in
  [%test_eq: string] actual expected

let%expect_test "input all, peek first (crlf)" =
  let data = "hey y'all\r\nwhat's up?" in
  let fname, _ = write_tmp_file data in
  (* let expected =
   *   Stdio.In_channel.with_file fname ~f:Stdio.In_channel.input_all
   * in *)
  print_endline
  @@ Ic.with_file fname ~f:(fun ic ->
         let l = Ic.peek_line ic in
         print_s @@ [%sexp_of: string option] l ;
         Ic.input_all ic ) ;
  [%expect {|
    ("hey y'all")
    hey y'all
    what's up? |}]

(* TODO would be nice to get these prop tests to avoid hitting the disk. *)

let lines_generator = QG.map (QG.list QG.string) ~f:(String.concat ~sep:"\n")

let lines_test f =
  let config = {Q.Test.default_config with test_count= trials} in
  Q.Test.run_exn ~config ~f
    ( module struct
      type t = string [@@deriving sexp]

      let quickcheck_generator = lines_generator

      let quickcheck_shrinker = Q.Shrinker.atomic
    end )

let%test_unit "read_all" =
  lines_test (fun data ->
      let fname, _ = write_tmp_file data in
      let expected = Stdio.In_channel.read_all fname in
      let actual = Ic.read_all fname in
      [%test_eq: string] actual expected )

(* Uses fold_lines internally *)
let%test_unit "iter_lines" =
  lines_test (fun data ->
      let fname, _ = write_tmp_file data in
      let expected = ref 0 in
      let actual = ref 0 in
      Stdio.In_channel.with_file fname ~f:(fun t ->
          Stdio.In_channel.iter_lines t ~f:(fun line ->
              expected := !expected + String.length line ) ) ;
      Ic.with_file fname ~f:(fun t ->
          Ic.iter_lines t ~f:(fun line ->
              actual := !actual + String.length line ) ) ;
      [%test_eq: int] !actual !expected )

let%test_unit "iter_lines with peek" =
  lines_test (fun data ->
      let fname, _ = write_tmp_file data in
      let expected = ref 0 in
      let actual = ref 0 in
      Stdio.In_channel.with_file fname ~f:(fun t ->
          Stdio.In_channel.iter_lines t ~f:(fun line ->
              expected := !expected + String.length line ) ) ;
      Ic.with_file fname ~f:(fun t ->
          let _line = Ic.peek_char t in
          let _c = Ic.peek_char t in
          let _line = Ic.peek_char t in
          Ic.iter_lines t ~f:(fun line ->
              let _c = Ic.peek_char t in
              let _line = Ic.peek_char t in
              let _c = Ic.peek_char t in
              actual := !actual + String.length line ) ) ;
      [%test_eq: int] !actual !expected )

let%test_unit "getting lines and peek" =
  lines_test (fun data ->
      let fname, _ = write_tmp_file data in
      let expected_lines =
        Stdio.In_channel.with_file fname ~f:(fun t ->
            Stdio.In_channel.input_lines t )
      in
      let actual_lines =
        Ic.with_file fname ~f:(fun t ->
            let _line = Ic.peek_char t in
            let _c = Ic.peek_char t in
            let _line = Ic.peek_char t in
            List.rev
            @@ Ic.fold_lines t ~init:[] ~f:(fun lines line ->
                   let _c = Ic.peek_char t in
                   let _line = Ic.peek_char t in
                   let _c = Ic.peek_char t in
                   line :: lines ) )
      in
      [%test_eq: string list] actual_lines expected_lines )

let%test_unit "read_lines" =
  lines_test (fun data ->
      let fname, _ = write_tmp_file data in
      let expected = Stdio.In_channel.read_lines fname in
      let actual = Ic.read_lines fname in
      [%test_eq: string list] actual expected )

let%test_unit "input_lines no fix eol" =
  lines_test (fun data ->
      let fname, _ = write_tmp_file data in
      let expected =
        Stdio.In_channel.with_file fname
          ~f:(Stdio.In_channel.input_lines ~fix_win_eol:false)
      in
      let actual = Ic.with_file fname ~f:(Ic.input_lines ~fix_win_eol:false) in
      [%test_eq: string list] actual expected )
