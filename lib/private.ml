(** Internally used modules *)

open! Base

(** Alternate interface to [Stdio.In_channel]. The [create] and [with_file]
    functions don't have a [binary] option, so it is compatible with the
    [Record_in_channel.Make] functor. *)
module In_channel : sig
  include module type of Stdio.In_channel

  val create : string -> t
  val with_file : string -> f:(t -> 'a) -> 'a
end = struct
  include Stdio.In_channel

  let create s = create ~binary:false s
  let with_file s ~f = with_file ~binary:false s ~f
end

(** A wrapper of Jane Street's [Stdio.In_channel]. Add's [peek_char] and
    [peek_line] functions that work on [fifo]s as well as regular files.

    Used internally for bio input channels so that you can pipe directly from
    gzip even in channels that need peeking.

    Differences from [Stdio.In_channel]

    - No binary mode

    Some functions are not implemented.

    - input
    - really_input
    - really_input_exn
    - input_char
    - input_byte
    - input_binary_int
    - unsafe_input_value
    - input_buffer
    - seek
    - pos
    - length
    - set_binary_mode_out *)
module Peekable_in_channel : sig
  type t [@@deriving equal]

  val stdin : t
  val create : string -> t
  val close : t -> unit
  val with_file : string -> f:(t -> 'a) -> 'a
  val input_all : t -> string
  val input_line : ?fix_win_eol:bool -> t -> string option
  val input_line_exn : ?fix_win_eol:bool -> t -> string

  val fold_lines :
    ?fix_win_eol:bool -> t -> init:'a -> f:('a -> string -> 'a) -> 'a

  val input_lines : ?fix_win_eol:bool -> t -> string list
  val iter_lines : ?fix_win_eol:bool -> t -> f:(string -> unit) -> unit
  val read_lines : ?fix_win_eol:bool -> string -> string list
  val read_all : string -> string

  (** Both [peek] functions are safe to call in the context of one of the
      iterator functions. *)

  val peek_char : ?fix_win_eol:bool -> t -> char option
  val peek_line : ?fix_win_eol:bool -> t -> string option
end = struct
  (* TODO let user set newline. *)

  (* Note that the buf is a line, and the input_line functions remove the
     newline, so the buf won't have a newline. *)
  type t = { ic : Stdio.In_channel.t; buf : string option ref }
  [@@deriving equal]

  let stdin = { ic = Stdio.In_channel.stdin; buf = ref None }
  let create fname = { ic = Stdio.In_channel.create fname; buf = ref None }

  let close { ic; buf } =
    Stdio.In_channel.close ic;
    buf := None

  let with_file fname ~f = Exn.protectx (create fname) ~f ~finally:close

  (* Input lines don't actually fill the buffer...you only need to fill a buffer
     if you're peeking. *)

  let input_all { ic; buf } =
    (* We use 65536 because that is the size of OCaml's IO buffers. *)
    let chunk_size = 65536 in
    let buffer = Buffer.create chunk_size in
    let () =
      match !buf with
      | None -> ()
      | Some line -> Buffer.add_string buffer (line ^ "\n")
    in
    let rec loop () =
      Stdlib.Buffer.add_channel buffer ic chunk_size;
      loop ()
    in
    try loop () with End_of_file -> Buffer.contents buffer

  let input_line ?(fix_win_eol = true) { ic; buf } =
    match !buf with
    | None -> Stdio.In_channel.input_line ~fix_win_eol ic
    | Some line ->
        buf := None;
        Some line

  let input_line_exn ?(fix_win_eol = true) { ic; buf } =
    match !buf with
    | None -> Stdio.In_channel.input_line_exn ~fix_win_eol ic
    | Some line ->
        buf := None;
        line

  let fold_lines ?(fix_win_eol = true) t ~init ~f =
    let rec loop ac =
      match input_line ~fix_win_eol t with
      | None -> ac
      | Some line -> loop (f ac line)
    in
    loop init

  let input_lines ?(fix_win_eol = true) t =
    List.rev
      (fold_lines ~fix_win_eol t ~init:[] ~f:(fun lines line -> line :: lines))

  let iter_lines ?(fix_win_eol = true) t ~f =
    fold_lines ~fix_win_eol t ~init:() ~f:(fun () line -> f line)

  let read_lines ?(fix_win_eol = true) fname =
    with_file fname ~f:(input_lines ~fix_win_eol)

  let read_all fname = with_file fname ~f:input_all

  let peek_char ?(fix_win_eol = true) { ic; buf } =
    match !buf with
    | None -> (
        (* No line in the buffer so get a new one. *)
        match Stdio.In_channel.input_line ~fix_win_eol ic with
        | None -> None
        | Some line -> (
            buf := Some line;
            match line with "" -> Some '\n' | line -> Some (String.get line 0)))
    | Some "" -> Some '\n'
    | Some line -> Some (String.get line 0)

  let peek_line ?(fix_win_eol = true) { ic; buf } =
    match !buf with
    | None -> (
        (* No line in the buffer so get a new one. *)
        match Stdio.In_channel.input_line ~fix_win_eol ic with
        | None -> None
        | Some line ->
            buf := Some line;
            Some line)
    | Some line -> Some line
end

(* Heavily influenced by Jane Street's Stdio.In_channel module. The license for
   that code is the following:

   The MIT License

   Copyright (c) 2016--2021 Jane Street Group, LLC opensource@janestreet.com

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
