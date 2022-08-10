open! Core
open Test_bio_io

let in_file = (Sys.get_argv ()).(1)
let out_file = (Sys.get_argv ()).(2)

let () =
  Out_channel.with_file out_file ~f:(fun oc ->
      let print = Out_channel.output_string oc in
      Btab_orig.In_channel.with_file_iter_records in_file ~f:(fun r ->
          print (Btab_orig.Record.to_string r ^ "\n")))
