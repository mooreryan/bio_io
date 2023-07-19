open! Core

let in_file = (Sys.get_argv ()).(1)

let out_file = (Sys.get_argv ()).(2)

let () =
  Out_channel.with_file out_file ~f:(fun oc ->
      let print = Out_channel.output_string oc in
      In_channel.with_file in_file ~f:(fun ic ->
          In_channel.iter_lines ic ~f:print ) )
