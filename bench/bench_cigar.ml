open! Core
open! Core_bench

let make_cigar () =
  let n = Random.int 100 in
  List.range 0 n
  |> List.map ~f:(fun _ ->
         let count = 1 + Random.int 1000 in
         let op = Array.random_element_exn [| "M"; "D"; "I" |] in
         Int.to_string count ^ op)
  |> String.concat ~sep:""

let cigars =
  let num_cigars = 1000 in
  List.range 0 num_cigars |> List.map ~f:(fun _i -> make_cigar ())

let () =
  let bench name f = Bench.Test.create ~name (fun () -> f ()) in
  Command_unix.run
    (Bench.make_command
       [
         bench "of_string" (fun () ->
             List.iter cigars ~f:(fun c ->
                 let _c = Bio_io.Cigar.of_string c in
                 ()));
         bench "of_string_exn" (fun () ->
             List.iter cigars ~f:(fun c ->
                 let _c = Bio_io.Cigar.of_string_exn c in
                 ()));
       ])
