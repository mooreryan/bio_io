open! Base
open! Bio_io.Btab_queries

let parse_argv () =
  match Sys.get_argv () with
  | [| _; file_name |] -> file_name
  | _ -> failwith "missing file_name"

let () =
  let file_name = parse_argv () in
  In_channel.with_file_iter_records file_name ~f:(fun r ->
      Stdio.print_endline "===";
      Stdio.print_endline @@ Record.query r;
      let hits = List.map ~f:Bio_io.Btab.Record.parse @@ Record.hits r in
      Stdio.print_s @@ [%sexp_of: Bio_io.Btab.Record.Parsed.t list] hits)
