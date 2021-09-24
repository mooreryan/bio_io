open! Base

exception Exn of string [@@deriving sexp]

type t [@@deriving equal, sexp]

val of_string_exn : string -> t
val of_string : string -> t Or_error.t

val to_string : t -> string

val ungapped_alignment_length : t -> int
