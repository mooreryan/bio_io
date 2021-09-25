(** A module for parsing CIGAR (Concise Idiosyncratic Gapped Alignment Report)
    strings.

    {1 Overview}

    A CIGAR string is a compressed representation of an aligment. Many programs
    use CIGAR strings including samtools, usearch, and MMseqs2. TODO links. *)

open! Base

(** {1 API} *)

type t [@@deriving equal, sexp]

exception Exn of string [@@deriving sexp]

val of_string_exn : string -> t
(** [of_string_exn string] parses string into a cigar [t]. Raises exceptions on
    parse error. Valid cigar strings look something like this: [1M2I3D5M]. *)

val of_string : string -> t Or_error.t
(** [of_stringstring] parses string into a cigar [t Or_error.t]. Should not
    raise exceptions. If [of_string] raises an exception, then you've found a
    bug. Valid cigar strings look something like this: [1M2I3D5M]. *)

val to_string : t -> string
(** [to_string t] converts a cigar from the internal representation to a
    printable string.

    {[
      let s = "1M2I3D5M" in
      assert (s = Cigar.to_string @@ Cigar.of_string_exn "1M2I3D5M")
    ]}

    Note that not all cigar strings will "round-trip" like the above example. If
    the input cigar string has no number specifying the length of a chunk, then
    that chunk is inferred to be of length 1. E.g.,

    {[
      let s = "MDIM" in
      assert ("1M1D1I1M" = Cigar.to_string @@ Cigar.of_string_exn "1M2I3D5M")
    ]} *)

val alignment_length : t -> int
(** [alignment_length t] returns the total length of the aligment as inferred by
    the Cigar string. E.g., [1M2I3D5M] -> 11.

    {[ assert (11 = Cigar.alignment_length @@ Cigar.of_string_exn "1M2I3D5M") ]} *)

val num_gaps : t -> int
(** [num_gaps t] returns the total number of gap columns in the alignment. E.g.,
    [1M2I3D5M] -> 5.

    {[ assert (5 = Cigar.num_gaps @@ Cigar.of_string_exn "1M2I3D5M") ]}*)

val num_matches : t -> int
(** [num_matches t] returns the number of matches in the alignment. E.g.,
    [1M2I3D5M] -> 6.

    {[ assert (6 = Cigar.num_matches @@ Cigar.of_string_exn "1M2I3D5M") ]}

    This is "ungapped alignment length" as in
    https://doi.org/10.1093/bioinformatics/bty262. Where ungapped is the
    alignment length minus number of gaps. I'm taking that to mean any position
    with a gap in either sequence is not counted. So the ungapped_length is the
    number of matches. *)

val query_length : t -> int
(** [query_length t] returns the length of the query/read within the boundaries
    of the alignment. This is the length of the query as inferred by the Cigar
    string. If it is a local aligment, it may not be equal to the length of the
    entire query sequence.

    {[ assert (8 = Cigar.query_length @@ Cigar.of_string_exn "1M2I3D5M") ]}*)

val target_length : t -> int
(** [target_length t] returns the length of the target/reference within the
    boundaries of the alignment. This is the length of the target as inferred by
    the Cigar string. If it is a local aligment, it may not be equal to the
    length of the entire target sequence.

    {[ assert (9 = Cigar.target_length @@ Cigar.of_string_exn "1M2I3D5M") ]} *)
