open! Core
open Test_bio_io

let in_file = (Sys.get_argv ()).(1)
let skip _ = ()
let () = Btab_orig.In_channel.with_file_iter_records in_file ~f:skip
