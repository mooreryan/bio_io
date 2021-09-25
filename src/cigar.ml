open! Base

(* Limited cigar format used by mmseqs.
   https://github.com/soedinglab/MMseqs2/wiki#alignment-format *)

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

let alignment_length cigar =
  List.fold cigar ~init:0 ~f:(fun length chunk -> length + Chunk.length chunk)

let num_gaps cigar =
  List.fold cigar ~init:0 ~f:(fun acc chunk ->
      match Chunk.op chunk with
      | Insertion | Deletion -> acc + Chunk.length chunk
      | Match -> acc)

(* This is as in https://doi.org/10.1093/bioinformatics/bty262. Where ungapped
   is the alignment length minus number of gaps. I'm taking that to mean any
   position with a gap in either sequence is not counted. So the ungapped_length
   is the number of matches. *)
let num_matches cigar =
  List.fold cigar ~init:0 ~f:(fun acc chunk ->
      match Chunk.op chunk with
      | Match -> acc + Chunk.length chunk
      | Insertion | Deletion -> acc)

let query_length cigar =
  List.fold cigar ~init:0 ~f:(fun length chunk ->
      match Chunk.op chunk with
      | Match | Insertion -> length + Chunk.length chunk
      | Deletion -> length)

let target_length cigar =
  List.fold cigar ~init:0 ~f:(fun length chunk ->
      match Chunk.op chunk with
      | Match | Deletion -> length + Chunk.length chunk
      | Insertion -> length)
