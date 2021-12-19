open! Base

module Record = struct
  type t = {
    query : string;
    target : string;
    fident : float;
    alnlen : int;
    mismatch : int;
    gapopen : int;
    qstart : int;
    qend : int;
    tstart : int;
    tend : int;
    evalue : float;
    bits : float;
  }
  [@@deriving fields, sexp]
  (** As of this commit ae7398d65f0c273d3d2c71fcf9bbd96dac695904, mmseqs2
      switches to [fident] as the default rather than [pident]. *)

  let of_string s =
    match String.split ~on:'\t' s with
    | [
     query;
     target;
     fident;
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
        Fields.create ~query ~target ~fident:(Float.of_string fident)
          ~alnlen:(Int.of_string alnlen) ~mismatch:(Int.of_string mismatch)
          ~gapopen:(Int.of_string gapopen) ~qstart:(Int.of_string qstart)
          ~qend:(Int.of_string qend) ~tstart:(Int.of_string tstart)
          ~tend:(Int.of_string tend) ~evalue:(Float.of_string evalue)
          ~bits:(Float.of_string bits)
    | _ -> failwith "Bad input"

  let to_string t =
    let conv to_s acc f = to_s (Field.get f t) :: acc in
    String.concat ~sep:"\t" @@ List.rev
    @@ Fields.fold ~init:[] ~query:(conv Fn.id) ~target:(conv Fn.id)
         ~fident:(conv Float.to_string) ~alnlen:(conv Int.to_string)
         ~mismatch:(conv Int.to_string) ~gapopen:(conv Int.to_string)
         ~qstart:(conv Int.to_string) ~qend:(conv Int.to_string)
         ~tstart:(conv Int.to_string) ~tend:(conv Int.to_string)
         ~evalue:(conv Float.to_string) ~bits:(conv Float.to_string)
end

module In_channel : sig
  include Record_in_channel.S with type record := Record.t
end = struct
  module T = struct
    include Private.In_channel

    type record = Record.t

    let input_record_exn ic =
      Option.map ~f:Record.of_string @@ Stdio.In_channel.input_line ic
  end

  include T
  include Record_in_channel.Make (T)
end
