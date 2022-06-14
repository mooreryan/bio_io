open! Base

module Record : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val query : t -> string
  val target : t -> string
  val pident : t -> float
  val alnlen : t -> int
  val mismatch : t -> int
  val gapopen : t -> int
  val qstart : t -> int
  val qend : t -> int
  val tstart : t -> int
  val tend : t -> int
  val evalue : t -> float
  val bits : t -> float
  val qlen : t -> int option
  val tlen : t -> int option

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
    [@@deriving sexp]
  end

  val parse : t -> Parsed.t
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
    [@@deriving sexp]
  end

  (* Generally prefer the direct functions. But sometimes you really do just
     want to have a record with human readable elements for whatever reason. *)
  let parse t =
    Parsed.
      {
        query = query t;
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

module In_channel : sig
  include Record_in_channel.S with type record := Record.t
end = struct
  module T = struct
    include Private.In_channel

    type record = Record.t

    let input_record_exn ic = Option.map ~f:Record.of_string @@ input_line ic
  end

  include T
  include Record_in_channel.Make (T)
end
