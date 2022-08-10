open! Base
open! Bio_io.Fasta

let () =
  Exn.protectx In_channel.stdin ~finally:In_channel.close ~f:(fun ic ->
      In_channel.iteri_records ic ~f:(fun i r ->
          Stdio.printf "%d -- %s -- %s\n" i (Record.id r) (Record.seq r)))
