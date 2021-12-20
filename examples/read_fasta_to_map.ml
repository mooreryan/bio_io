(* Read FASTA records from a file and put them in a map. The sequence ID will be
   the key and the sequence itself will be the data. *)

open! Base
open! Bio_io.Fasta

let parse_argv () =
  match Sys.get_argv () with
  | [| _; file_name |] -> file_name
  | _ -> failwith "missing file_name"

let () =
  let file_name = parse_argv () in
  let _seq_map =
    In_channel.with_file_fold_records_exn file_name
      ~init:(Map.empty (module String))
      ~f:(fun m r ->
        let open Record in
        (* This will raise an exception if the sequence ID is duplicated in the
           input file. *)
        Map.add_exn m ~key:(id r) ~data:(seq r))
  in
  (* Do some stuff with the sequence map. *)
  ()
