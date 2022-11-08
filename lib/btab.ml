(** Parsing tab-delimited homology search results (btab, blast-tab, BLAST outfmt
    6)

    Each query-target alignment (hit) is a single record. See {!Btab_queries} if
    you want to treat queries as records.

    {1 Example}

    Here is a small example program.

    {[
      open! Base

      let parse_argv () =
        match Sys.get_argv () with
        | [| _; file_name |] -> file_name
        | _ -> failwith "missing file_name"

      let file_name = parse_argv ()

      let () =
        let open Bio_io.Btab in
        In_channel.with_file_iter_records file_name ~f:(fun r ->
            Stdio.printf "%s => %s (%.3f)\n" (Record.query r) (Record.target r)
              (Record.bits r))
    ]} *)

open! Base

(** {1 Modules} *)

(** A record type for Btab (Blast-tab) files

    {1 Overview}

    Each record represents a "hit", or one query-target pair as returned by the
    homology search tool.

    These records are output by many homology search tools. It is in the style
    of BLAST's `outfmt 6`, but with names that match MMseqs2.

    Additionally, it handles cases in which query length and target length are
    included (in the style of [mmseqs easy-search --format-mode 2]). *)
module Record : sig
  (** {1 API} *)

  type t [@@deriving sexp_of]

  (** {2 Creating records} *)

  val of_string : string -> t
  (** [of_string s] creates a new [t] from tab-delimeted string [s]. Will raise
      if the format of [s] is bad.

      Valid strings are tab-delimited with either 12 or 14 fields.

      The 12 field variants fields should be [query], [target], [pident],
      [alnlen], [mismatch], [gapopen], [qstart], [qend], [tstart], [tend],
      [evalue], and [bits], in that order. It is in the style of `outfmt 6` from
      BLAST and related tools.

      The 14 field variant should have the same 12 fields plus [qlen] and
      [tlen]. It is like the output of [mmseqs] with [--format-mode 2]. *)

  val to_string : t -> string
  (** [to_string t] creates a new "ready to print" tab-delimited string
      representation of [t]. *)

  (** {2 Accessing fields} *)

  val query : t -> string
  (** [query t] returns the query sequence of the search record. *)

  val target : t -> string
  (** [target t] returns the target (a.k.a subject) sequence of the search
      record. *)

  val pident : t -> float
  (** [pident t] returns the percent identity of the hit.

      Note that no processing is done. So if your software returns a percentage
      from 0 to 100, this value will be from 0 to 100. If your software returns
      a fraction from 0 to 1, then this value will range from 0 to 1. *)

  val alnlen : t -> int
  (** [alnlen t] returns the length of the alignment. *)

  val mismatch : t -> int
  (** [mismatch t] returns the number of mismatches. *)

  val gapopen : t -> int
  (** [gapopen t] returns the number of gap openings. *)

  val qstart : t -> int
  (** [qstart t] returns the start position of the aligment in the query
      sequence.

      Note that no processing is done. If the input file has 1-based
      coordinates, then this will also have 1-based coordinates. This also holds
      for [qend], [tstart], and [tend]. *)

  val qend : t -> int
  (** [qend t] returns the end position of the aligment in the query sequence. *)

  val tstart : t -> int
  (** [tstart t] returns the start position of the aligment in the target
      sequence. *)

  val tend : t -> int
  (** [tend t] returns the end position of the aligment in the target sequence. *)

  val evalue : t -> float
  (** [evalue t] returns the expect value of the hit. *)

  val bits : t -> float
  (** [evalue t] returns the bit-score of the hit. *)

  val qlen : t -> int option
  (** [qlen t] returns the length of the query sequence if it was present in the
      input file. *)

  val tlen : t -> int option
  (** [tlen t] returns the length of the target sequence if it was present in
      the input file. *)

  (** {2 Parsed records} *)

  (** A fully parsed Btab record.

      Sometimes, you may want to fully parse the [Btab.Record] into an OCaml
      [record], e.g., for s-expression serialization.

      [Btab.Record] is not parsed into an OCaml [record] by default. Rather, the
      fields you need are generated on the fly using string indexing. This is a
      lot faster, but like I mentioned before, sometimes you really do want the
      whole thing pre-parsed. Use this module in those cases. *)
  module Parsed : sig
    type t = {
      query : string;
      target : string;
      pident : float;
      alnlen : int;
      mismatch : int;
      gapopen : int;
      qstart : int;
      qend : int;
      tstart : int;
      tend : int;
      evalue : float;
      bits : float;
      qlen : int option;
      tlen : int option;
    }
    [@@deriving equal, fields, sexp]
  end

  val parse : t -> Parsed.t
  (** [parse t] parses the [Btab.Record.t] into [Btab.Record.Parsed.t]. *)
end = struct
  type t = {
    line : string;
    query : int * int;
    target : int * int;
    pident : int * int;
    alnlen : int * int;
    mismatch : int * int;
    gapopen : int * int;
    qstart : int * int;
    qend : int * int;
    tstart : int * int;
    tend : int * int;
    evalue : int * int;
    bits : int * int;
    qlen : (int * int) option;
    tlen : (int * int) option;
  }
  [@@deriving sexp]

  (* Store the starting indices and the offsets for speedier parsing. Also,
     store the original line so that we can slice it. *)
  let of_string s =
    let start = ref 0 in
    let l = ref [] in
    let len = String.length s in
    for i = 0 to len - 1 do
      let c = String.unsafe_get s i in
      if Char.(c = '\t') then (
        l := (!start, i - !start) :: !l;
        start := i + 1)
    done;
    (* Catch the last token. *)
    l := (!start, len - !start) :: !l;
    match List.rev !l with
    | [
     query;
     target;
     pident;
     alnlen;
     mismatch;
     gapopen;
     qstart;
     qend;
     tstart;
     tend;
     evalue;
     bits;
     qlen;
     tlen;
    ] ->
        {
          line = s;
          query;
          target;
          pident;
          alnlen;
          mismatch;
          gapopen;
          qstart;
          qend;
          tstart;
          tend;
          evalue;
          bits;
          qlen = Some qlen;
          tlen = Some tlen;
        }
    | [
     query;
     target;
     pident;
     alnlen;
     mismatch;
     gapopen;
     qstart;
     qend;
     tstart;
     tend;
     evalue;
     bits;
    ] ->
        {
          line = s;
          query;
          target;
          pident;
          alnlen;
          mismatch;
          gapopen;
          qstart;
          qend;
          tstart;
          tend;
          evalue;
          bits;
          qlen = None;
          tlen = None;
        }
    | _ -> failwith ("Bad btab line: '" ^ s ^ "'")

  let to_string t = t.line

  let query t =
    let pos, len = t.query in
    String.sub t.line ~pos ~len

  let target t =
    let pos, len = t.target in
    String.sub t.line ~pos ~len

  let pident t =
    let pos, len = t.pident in
    Float.of_string @@ String.sub t.line ~pos ~len

  let alnlen t =
    let pos, len = t.alnlen in
    Int.of_string @@ String.sub t.line ~pos ~len

  let mismatch t =
    let pos, len = t.mismatch in
    Int.of_string @@ String.sub t.line ~pos ~len

  let gapopen t =
    let pos, len = t.gapopen in
    Int.of_string @@ String.sub t.line ~pos ~len

  let qstart t =
    let pos, len = t.qstart in
    Int.of_string @@ String.sub t.line ~pos ~len

  let qend t =
    let pos, len = t.qend in
    Int.of_string @@ String.sub t.line ~pos ~len

  let tstart t =
    let pos, len = t.tstart in
    Int.of_string @@ String.sub t.line ~pos ~len

  let tend t =
    let pos, len = t.tend in
    Int.of_string @@ String.sub t.line ~pos ~len

  let evalue t =
    let pos, len = t.evalue in
    Float.of_string @@ String.sub t.line ~pos ~len

  let bits t =
    let pos, len = t.bits in
    Float.of_string @@ String.sub t.line ~pos ~len

  let qlen t =
    Option.map t.qlen ~f:(fun (pos, len) ->
        Int.of_string @@ String.sub t.line ~pos ~len)

  let tlen t =
    Option.map t.tlen ~f:(fun (pos, len) ->
        Int.of_string @@ String.sub t.line ~pos ~len)

  (* Sometimes you really do just want to get a record with everything already
     parsed out. *)
  module Parsed = struct
    type t = {
      query : string;
      target : string;
      pident : float;
      alnlen : int;
      mismatch : int;
      gapopen : int;
      qstart : int;
      qend : int;
      tstart : int;
      tend : int;
      evalue : float;
      bits : float;
      qlen : int option;
      tlen : int option;
    }
    [@@deriving equal, fields, sexp]
  end

  (* Generally prefer the direct functions. But sometimes you really do just
     want to have a record with human readable elements for whatever reason. *)
  let parse t =
    {
      Parsed.query = query t;
      target = target t;
      pident = pident t;
      alnlen = alnlen t;
      mismatch = mismatch t;
      gapopen = gapopen t;
      qstart = qstart t;
      qend = qend t;
      tstart = tstart t;
      tend = tend t;
      evalue = evalue t;
      bits = bits t;
      qlen = qlen t;
      tlen = tlen t;
    }
end

(** [In_channel] for Btab records.

    For more general info, see the {!Bio_io.Record_in_channel} module mli file.

    For some examples, see {!Bio_io.Fasta.In_channel}. Those are for the FASTA
    files, but the API is the same.. *)
module In_channel : sig
  include Record_in_channel.S with type record := Record.t
end = struct
  module T = struct
    include Private.In_channel

    type record = Record.t

    let input_record ic = Option.map ~f:Record.of_string @@ input_line ic
  end

  include T
  include Record_in_channel.Make (T)
end
