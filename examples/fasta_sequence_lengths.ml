open! Base

let fasta_file = (Sys.get_argv ()).(1)

let () =
  (* This open gives you [In_channel] and [Record]. *)
  let open Bio_io.Fasta in
  In_channel.with_file_iter_records fasta_file ~f:(fun record ->
      (* Print the ID and the length of the sequence. *)
      Stdio.printf "%s => %d\n" (Record.id record) (Record.seq_length record) )
