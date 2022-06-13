open! Base

(* Limited cigar format used by mmseqs.
   https://github.com/soedinglab/MMseqs2/wiki#alignment-format *)

exception Int_overflow of (int * int) [@@deriving sexp]

let add_exn x y =
  let sum = x + y in
  let sign_x = Int.sign x in
  if Sign.(sign_x = Int.sign y && sign_x <> Int.sign sum) then
    raise (Int_overflow (x, y))
  else sum

exception Exn of string [@@deriving sexp]

(** An operation describing an alignment.

    - [Match]: A non-gap alignment position. May be a mismatch.
    - [Insertion]: Gap in the target/reference sequence.
    - [Deletion]: Gap in the query/read sequence.

    (Note that the drive5 webpage has this backwards. It's possible they have
    reversed definition of query and target.) *)
type op = Match | Insertion | Deletion [@@deriving equal, sexp]

let op_of_char = function
  | 'M' -> Or_error.return Match
  | 'I' -> Or_error.return Insertion
  | 'D' -> Or_error.return Deletion
  | c -> Or_error.errorf "Expected M, D, or I. Got %c." c

let op_to_char = function Match -> 'M' | Insertion -> 'I' | Deletion -> 'D'
let op_to_string = function Match -> "M" | Insertion -> "I" | Deletion -> "D"

module Chunk : sig
  (** Mint a new type here so the lengths are guaranteed to be good. *)

  type t [@@deriving equal, sexp]

  val create : int -> op -> t Or_error.t
  val to_string : t -> string
  val length : t -> int
  val op : t -> op
end = struct
  type t = int * op [@@deriving equal, sexp]

  let create length op =
    if length > 0 then Or_error.return (length, op)
    else Or_error.errorf "Length must be > 0.  Got %d." length

  let to_string (length, op) = Int.to_string length ^ op_to_string op
  let length (length, _op) = length
  let op (_length, op) = op
end

type t = Chunk.t list [@@deriving equal, sexp]

let is_int = function
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

let is_cigar_op = function 'M' | 'D' | 'I' -> true | _ -> false

(* One weird thing is that back to back same ops are allowed. See tests. *)
let of_string_exn s =
  let _, pairs =
    String.fold s ~init:("", []) ~f:(fun (current, all) c ->
        match (is_int c, is_cigar_op c) with
        | true, true ->
            (* Can't be both int and cigar_op *)
            assert false
        | false, false ->
            raise (Exn ("Expected int or operation. Got " ^ Char.to_string c))
        | false, true ->
            (* You can get an Op without a preceding integer. In this case,
               treat the length as 1. See
               https://drive5.com/usearch/manual/cigar.html: "In some CIGAR
               variants, the integer may be omitted if it is 1." *)
            let length =
              if String.(current = "") then 1 else Int.of_string current
            in
            let op = Or_error.ok_exn @@ op_of_char c in
            (* ok_exn okay here as the parsing disallows negative numbers. *)
            let chunk = Or_error.ok_exn @@ Chunk.create length op in
            ("", chunk :: all)
        | true, false -> (current ^ String.of_char c, all))
  in
  List.rev pairs

let of_string s = Utils.try1' ~msg:"Error parsing cigar string" of_string_exn s
let to_string cigar = String.concat ~sep:"" @@ List.map cigar ~f:Chunk.to_string

let alignment_length_exn cigar =
  List.fold cigar ~init:0 ~f:(fun length chunk ->
      add_exn length (Chunk.length chunk))

let alignment_length cigar = Utils.try1 alignment_length_exn cigar

let num_gaps_exn cigar =
  List.fold cigar ~init:0 ~f:(fun acc chunk ->
      match Chunk.op chunk with
      | Insertion | Deletion -> add_exn acc (Chunk.length chunk)
      | Match -> acc)

let num_gaps cigar = Utils.try1 num_gaps_exn cigar

(* This is as in https://doi.org/10.1093/bioinformatics/bty262. Where ungapped
   is the alignment length minus number of gaps. I'm taking that to mean any
   position with a gap in either sequence is not counted. So the ungapped_length
   is the number of matches. *)
let num_matches_exn cigar =
  List.fold cigar ~init:0 ~f:(fun acc chunk ->
      match Chunk.op chunk with
      | Match -> add_exn acc (Chunk.length chunk)
      | Insertion | Deletion -> acc)

let num_matches cigar = Utils.try1 num_matches_exn cigar

let query_length_exn cigar =
  List.fold cigar ~init:0 ~f:(fun length chunk ->
      match Chunk.op chunk with
      | Match | Insertion -> add_exn length (Chunk.length chunk)
      | Deletion -> length)

let query_length cigar = Utils.try1 query_length_exn cigar

let target_length_exn cigar =
  List.fold cigar ~init:0 ~f:(fun length chunk ->
      match Chunk.op chunk with
      | Match | Deletion -> add_exn length (Chunk.length chunk)
      | Insertion -> length)

let target_length cigar = Utils.try1 target_length_exn cigar

(* "Drawing" functions and helpers *)

let op_to_target_draw_char ?(gap = '-') ?(non_gap = '-') op =
  match op with Match | Deletion -> non_gap | Insertion -> gap

let op_to_query_draw_char ?(gap = '-') ?(non_gap = '-') op =
  match op with Match | Insertion -> non_gap | Deletion -> gap

let op_to_op_draw_char = op_to_char

(* "Draw" a cigar string by converting the operations to the proper char
   representation. Takes a [op_to_char_fun] so that you can use it for queries,
   targets, and operation strings by passing the appropriate function. *)
let draw_helper op_to_char_fun cigar =
  String.concat ~sep:""
  @@ List.map cigar ~f:(fun chunk ->
         let c = op_to_char_fun @@ Chunk.op chunk in
         (* WARNING: this can raise an Out of memory error depending on the
            cigar string :) *)
         String.make (Chunk.length chunk) c)

let char_list_to_string cl =
  String.concat ~sep:"" @@ List.map cl ~f:Char.to_string

(** Take a [length] and a string [s], and break it up into [length] size splits.
    The last split will have length <= [length]. *)
let string_splits length s =
  List.map ~f:char_list_to_string @@ List.chunks_of ~length @@ String.to_list s

(* Will raise if strings are different length. *)
let wrap_aligment max_len ~target ~target_label ~query ~query_label ~op
    ~op_label =
  let string_splits' = string_splits max_len in
  (* Use _exn here as the strings passed in should have the same length from the
     caller. *)
  String.concat ~sep:"\n\n"
  @@ List.map3_exn (string_splits' target) (string_splits' query)
       (string_splits' op) ~f:(fun target_split query_split op_split ->
         String.concat ~sep:"\n"
           [
             target_label ^ target_split;
             query_label ^ query_split;
             op_label ^ op_split;
           ])

let draw_exn ?(max_aln_len = 1000) ?(gap = '-') ?(non_gap = 'X') ?(wrap = 60)
    cigar =
  let len = alignment_length_exn cigar in
  assert (len >= 0);
  if len > max_aln_len then ""
  else
    let draw_target = draw_helper @@ op_to_target_draw_char ~gap ~non_gap in
    let draw_query = draw_helper @@ op_to_query_draw_char ~gap ~non_gap in
    let draw_op = draw_helper op_to_op_draw_char in
    let target_label = "t: " in
    let query_label = "q: " in
    let op_label = "o: " in
    let target = draw_target cigar in
    let query = draw_query cigar in
    let op = draw_op cigar in
    wrap_aligment wrap ~target ~target_label ~query ~query_label ~op ~op_label

let draw ?(max_aln_len = 1000) ?(gap = '-') ?(non_gap = 'X') ?(wrap = 60) cigar
    =
  match draw_exn ~max_aln_len ~gap ~non_gap ~wrap cigar with
  | exception exn ->
      Or_error.error "Couldn't calculate alignment length" exn Exn.sexp_of_t
  | result -> Or_error.return result
