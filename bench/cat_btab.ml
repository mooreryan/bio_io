open! Core
open Bio_io

let in_file = (Sys.get_argv ()).(1)
let out_file = (Sys.get_argv ()).(2)

let () =
  Out_channel.with_file out_file ~f:(fun oc ->
      let print = Out_channel.output_string oc in
      Btab.In_channel.with_file_iter_records_exn in_file ~f:(fun r ->
          print (Btab.Record.to_string r ^ "\n")))
