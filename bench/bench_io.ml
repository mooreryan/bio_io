open! Core
open! Core_bench
open Bio_io

let infile = Sys.getenv_exn "BENCH_IO_INFILE"

module Ic = Private.Peekable_in_channel

let () =
  let bench name = Bench.Test.create ~name in
  Command_unix.run
    (Bench.make_command
       [
         bench "ic_read_all" (fun () -> ignore (Ic.read_all infile));
         bench "stdio_read_all" (fun () ->
             ignore (Stdio.In_channel.read_all infile));
         bench "ic_fold_lines" (fun () ->
             ignore
               (Ic.with_file infile ~f:(fun t ->
                    Ic.fold_lines t ~init:0 ~f:(fun count _line -> count + 1))));
         bench "ic_fold_lines_with_peek_char" (fun () ->
             ignore
               (Ic.with_file infile ~f:(fun t ->
                    Ic.fold_lines t ~init:0 ~f:(fun count _line ->
                        let _ = Ic.peek_char t in
                        count + 1))));
         bench "ic_fold_lines_with_peek_line" (fun () ->
             ignore
               (Ic.with_file infile ~f:(fun t ->
                    Ic.fold_lines t ~init:0 ~f:(fun count _line ->
                        let _ = Ic.peek_line t in
                        count + 1))));
         bench "stdio_fold_lines" (fun () ->
             ignore
               (Stdio.In_channel.with_file infile ~f:(fun t ->
                    Stdio.In_channel.fold_lines t ~init:0 ~f:(fun count _line ->
                        count + 1))));
         bench "ic_iter_lines" (fun () ->
             ignore
               (Ic.with_file infile ~f:(fun t ->
                    Ic.iter_lines t ~f:(fun line -> prerr_endline line))));
         bench "stdio_iter_lines" (fun () ->
             ignore
               (Stdio.In_channel.with_file infile ~f:(fun t ->
                    Stdio.In_channel.iter_lines t ~f:(fun line ->
                        prerr_endline line))));
       ])
