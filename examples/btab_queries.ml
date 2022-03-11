open! Base
open! Bio_io.Btab_queries

let parse_argv () =
  match Sys.get_argv () with
  | [| _; file_name |] -> file_name
  | _ -> failwith "missing file_name"

let () =
  let file_name = parse_argv () in
  In_channel.with_file_iter_records_exn file_name ~f:(fun r ->
      Stdio.print_s @@ Record.sexp_of_t r)
