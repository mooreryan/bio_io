open! Core
open Bio_io

let in_file = (Sys.get_argv ()).(1)
let skip _ = ()
let () = Btab.In_channel.with_file_iter_records_exn in_file ~f:skip
