open! Core

let in_file = (Sys.get_argv ()).(1)

let skip _ = ()

let () =
  In_channel.with_file in_file ~f:(fun ic -> In_channel.iter_lines ic ~f:skip)
