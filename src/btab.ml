open! Base

module Record = struct
  type t = {
    qaccver : string;
    saccver : string;
    pident : float;
    length : int;
    mismatch : int;
    gapopen : int;
    qstart : int;
    qend : int;
    sstart : int;
    send : int;
    evalue : float;
    bitscore : float;
  }
  [@@deriving fields, sexp]

  let of_string s =
    match String.split ~on:'\t' s with
    | [
     qaccver;
     saccver;
     pident;
     length;
     mismatch;
     gapopen;
     qstart;
     qend;
     sstart;
     send;
     evalue;
     bitscore;
    ] ->
        Fields.create ~qaccver ~saccver ~pident:(Float.of_string pident)
          ~length:(Int.of_string length) ~mismatch:(Int.of_string mismatch)
          ~gapopen:(Int.of_string gapopen) ~qstart:(Int.of_string qstart)
          ~qend:(Int.of_string qend) ~sstart:(Int.of_string sstart)
          ~send:(Int.of_string send) ~evalue:(Float.of_string evalue)
          ~bitscore:(Float.of_string bitscore)
    | _ -> failwith "Bad input"

  let to_string t =
    let conv to_s acc f = to_s (Field.get f t) :: acc in
    String.concat ~sep:"\t" @@ List.rev
    @@ Fields.fold ~init:[] ~qaccver:(conv Fn.id) ~saccver:(conv Fn.id)
         ~pident:(conv Float.to_string) ~length:(conv Int.to_string)
         ~mismatch:(conv Int.to_string) ~gapopen:(conv Int.to_string)
         ~qstart:(conv Int.to_string) ~qend:(conv Int.to_string)
         ~sstart:(conv Int.to_string) ~send:(conv Int.to_string)
         ~evalue:(conv Float.to_string) ~bitscore:(conv Float.to_string)
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
