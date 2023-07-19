open! Base

let robust_comparison_tolerance = 1E-7

let robustly_compare x y =
  let open Poly in
  let d = Stdlib.( -. ) x y in
  if d < Stdlib.( ~-. ) robust_comparison_tolerance then -1
  else if d > robust_comparison_tolerance then 1
  else 0

let equal x y = Int.equal 0 @@ robustly_compare x y
let ( = ) = equal

(* This code is from Jane Street's Core v0.15. Orig license:

   The MIT License

   Copyright (c) 2008--2022 Jane Street Group, LLC <opensource@janestreet.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)
