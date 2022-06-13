open! Core
open Bio_io

(* TODO adapt the benches from the btab_mechanisms repo. It has more realistic
   usage for benching. *)

let s =
  "Q \
   1\tq1t1\t0.1\t22\t333\t4444\t55555\t666666\t7777777\t88888888\t9.99E-05\t1000\t1\t2"

let () =
  (* let open Lib in *)
  let bench name f = Core_bench.Bench.Test.create ~name f in
  Command_unix.run
    (Core_bench.Bench.make_command
       [
         bench "btab (ignore)" (fun () -> Btab.Record.of_string s);
         bench "btab_orig (1)" (fun () ->
             Test_bio_io.Btab_orig.Record.of_string s);
         bench "btab_orig (2)" (fun () ->
             Test_bio_io.Btab_orig.Record.of_string s);
         bench "btab (3)" (fun () -> Btab.Record.of_string s);
         bench "btab (4)" (fun () -> Btab.Record.of_string s);
       ])
