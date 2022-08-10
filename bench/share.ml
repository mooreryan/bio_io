open! Core
open Bio_io

let infile = Sys.getenv_exn "BENCH_FASTA_INFILE"
let get_record_list () = Fasta.In_channel.with_file_records infile

let total_length () =
  let total_length =
    Fasta.In_channel.with_file_fold_records infile ~init:0
      ~f:(fun length record ->
        let sequence = Fasta.Record.seq record in
        length + String.length sequence)
  in
  prerr_endline @@ "total bases: " ^ Int.to_string total_length

let print_records () =
  Fasta.In_channel.with_file_iter_records infile ~f:(fun record ->
      let open Fasta.Record in
      eprintf "%s => %d\n" (id record) (String.length (seq record)))

let print_recordsi () =
  Fasta.In_channel.with_file_iteri_records infile ~f:(fun index record ->
      let open Fasta.Record in
      eprintf "%d: %s => %d\n" (index + 1) (id record)
        (String.length (seq record)))
