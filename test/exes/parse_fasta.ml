open! Base
open! Bio_io.Fasta

let parse_argv () =
  match Sys.get_argv () with
  | [|_; file_name|] ->
      file_name
  | _ ->
      failwith "missing file_name"

let () =
  let file_name = parse_argv () in
  let num_seqs, total_bases =
    In_channel.with_file_fold_records file_name ~init:(0, 0)
      ~f:(fun (num_seqs, total_bases) r ->
        (num_seqs + 1, total_bases + Record.seq_length r) )
  in
  Stdio.printf "num seqs: %d, num bases: %d\n" num_seqs total_bases
