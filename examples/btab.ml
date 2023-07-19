open! Base

let parse_argv () =
  match Sys.get_argv () with
  | [|_; file_name|] ->
      file_name
  | _ ->
      failwith "missing file_name"

let file_name = parse_argv ()

let () =
  let open Bio_io.Btab in
  In_channel.with_file_iter_records file_name ~f:(fun r ->
      Stdio.printf "%s => %s (%.3f)\n" (Record.query r) (Record.target r)
        (Record.bits r) )
