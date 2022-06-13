(** A module for parsing CIGAR (Concise Idiosyncratic Gapped Alignment Report)
    strings.

    {1 Overview}

    A CIGAR string is a compressed representation of an aligment.

    Here is an example alignment of two sequences along wth the operations,
    Match (M), Insertion (I), and Deletion (D).

    {[
      target:     AGTG-TCTTTG
      query:      ACTGAACT--G
      operation:  MMMMIMMMDDM
    ]}

    Alignment columns with bases/residues in both the query and the target are
    considered a [Match] (even if there are two different bases/residues in the
    column). Columns with a gap in the target/reference sequence are labeled
    [Insertions], and columns with a gap in the query/read sequence are labeled
    [Deletions].

    The CIGAR string is formed by counting up the runs of operations of the same
    type. For the above alignment, the operations are [MMMMIMMMDDM], so the
    CIGAR string will be [4M1I3M2D1M].

    {2 Supported CIGAR Operations}

    - [Match]: A non-gap alignment position. May be a mismatch.
    - [Insertion]: Gap in the target/reference sequence.
    - [Deletion]: Gap in the query/read sequence.

    Many programs use CIGAR strings including
    {{:http://samtools.github.io/hts-specs/SAMv1.pdf} samtools},
    {{:https://drive5.com/usearch/manual/cigar.html} usearch}, and
    {{:https://github.com/soedinglab/MMseqs2/wiki#alignment-format} MMseqs2}.
    CIGAR strings can vary depending on the program that produces or consumes
    them, and not all versions are compatible.

    This implementation uses a reduced set of operations used by MMseqs2: Match
    (M), Insertion (I), and Deletion (D). Eventually, I will implement the rest
    of the operations, but in the meantime, if your use case requires the full
    CIGAR spec, please open an {{:https://github.com/mooreryan/bio_io/issues}
    issue} on GitHub and let me know!

    {2 Notes}

    - I refer to each number-letter pair as a chunk. Each chunk is a [length]
      and an [operation]. The length specifies the number of consecutive
      operations.
    - Some programs generate chunks without a [length]--just the [operation]. In
      these cases, the length is taken to be 1. So the following CIGAR strings
      will parse into the same data structure: [MIDM] and [1M1I1D1M].
    - Empty CIGAR strings are allowed and do not raise errors. *)

open! Base

(** {1 API} *)

exception Exn of string [@@deriving sexp]
exception Int_overflow of (int * int) [@@deriving sexp]

type t [@@deriving equal, sexp]

val of_string_exn : string -> t
(** [of_string_exn string] parses string into a cigar [t]. Raises exceptions on
    parse error. Valid cigar strings look something like this: [1M2I3D5M]. *)

val of_string : string -> t Or_error.t
(** [of_string string] parses string into a cigar [t Or_error.t]. Should not
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

(** {2:length_functions Length & count functions}

    The following functions return information about the aligment as inferred
    from the CIGAR string. Each of the examples uses this CIGAR string,
    [1M2I3D5M], which represents the following alignment.

    {[
      target: X--XXXXXXXX
      query:  XXX---XXXXX
      op:     MIIDDDMMMMM
    ]}

    Then "length" calculating functions can overflow on CIGAR strings describing
    genome alignments. In practice, this won't happen often as generally you
    will be parsing the results of local alignments, e.g., from BLAST, MMseqs2,
    or reading SAM files.

    Since it is possible to overflow integer arithmetic on CIGAR strings you may
    parse, the length functions use an addition function that raises
    [Int_overflow] exceptions rather than use {{:https://ocaml.org/api/Int.html}
    two's complement wrapping}.

    Thus, there are two versions, those that end in [_exn] that may raise
    [Int_overflow] and those that return [t Or_error.t], which catch
    [Int_overflow] and other exceptions. *)

val alignment_length_exn : t -> int

val alignment_length : t -> int Or_error.t
(** [alignment_length t] returns the total length of the aligment as inferred by
    the Cigar string.

    Here is the alignment for this CIGAR string:

    {[
      assert (11 = Cigar.alignment_length @@ Cigar.of_string_exn "1M2I3D5M")
    ]} *)

val num_gaps_exn : t -> int

val num_gaps : t -> int Or_error.t
(** [num_gaps t] returns the total number of gap columns in the alignment.

    {[
      assert (5 = Cigar.num_gaps @@ Cigar.of_string_exn "1M2I3D5M")
    ]}*)

val num_matches_exn : t -> int

val num_matches : t -> int Or_error.t
(** [num_matches t] returns the number of matches in the alignment.

    {[
      assert (6 = Cigar.num_matches @@ Cigar.of_string_exn "1M2I3D5M")
    ]}

    This is "ungapped alignment length" as in
    https://doi.org/10.1093/bioinformatics/bty262. Where ungapped is the
    alignment length minus number of gaps. I'm taking that to mean any position
    with a gap in either sequence is not counted. So the ungapped_length is the
    number of matches. *)

val query_length_exn : t -> int

val query_length : t -> int Or_error.t
(** [query_length t] returns the length of the query/read within the boundaries
    of the alignment. This is the length of the query as inferred by the Cigar
    string. If it is a local aligment, it may not be equal to the length of the
    entire query sequence.

    {[
      assert (8 = Cigar.query_length @@ Cigar.of_string_exn "1M2I3D5M")
    ]}*)

val target_length_exn : t -> int

val target_length : t -> int Or_error.t
(** [target_length t] returns the length of the target/reference within the
    boundaries of the alignment. This is the length of the target as inferred by
    the Cigar string. If it is a local aligment, it may not be equal to the
    length of the entire target sequence.

    {[
      assert (9 = Cigar.target_length @@ Cigar.of_string_exn "1M2I3D5M")
    ]} *)

(** {2 Drawing Functions}

    Drawing functions can also raise [Int_overflow] because they rely on the
    length functions. Use [draw] rather than [draw_exn] to catch errors. *)

val draw_exn :
  ?max_aln_len:int -> ?gap:char -> ?non_gap:char -> ?wrap:int -> t -> string

val draw :
  ?max_aln_len:int ->
  ?gap:char ->
  ?non_gap:char ->
  ?wrap:int ->
  t ->
  string Or_error.t
(** [draw t ~max_aln_len ~gap ~non_gap ~wrap] outputs a string representation of
    the aligmnent as inferred from the CIGAR string. This function is mainly
    just for fun or if you want to learn how CIGAR strings work :)

    - Use [max_aln_len] to prevent "drawing" alignments that are longer than
      this value. Currently, this function allocates a string approx. three
      times the entire size of the alignment. So keep this low... :) Default is
      [1000].
    - [gap] is the character to use for gaps. Default: ['-'].
    - [non_gap] is the character to use for non-gaps. Default ['X'].
    - [wrap] is the max length of the sequence to show in a line before
      wrapping. Default is [60].

    E.g., this [print_endline @@ draw "1M2I3D5M"]

    would print this:

    {[
      target: X--XXXXXXXX
      query:  XXX---XXXXX
      op:     MIIDDDMMMMM
    ]}

    {3 Wrapping long sequences}

    You can wrap long sequences so they don't take up so much horizontal space.
    This code

    {[
      print_endline @@ Cigar.draw ~wrap:10 @@ Cigar.of_string_exn "25M"
    ]}

    will print out

    {[
      t: XXXXXXXXXX
      q: XXXXXXXXXX
      o: MMMMMMMMMM

      t: XXXXXXXXXX
      q: XXXXXXXXXX
      o: MMMMMMMMMM

      t: XXXXX
      q: XXXXX
      o: MMMMM
    ]} *)
