open! Base

(** A record type for FASTA files.

    {1 Overview}

    {2 Example 1}

    If you have a fasta file something like this:

    {[
      >s1 apple pie
      ACTG
      actg
    ]}

    Then you would get a [record] something like this:

    {[
      Fasta_record.id record (* "s1" *) Fasta_record.desc record
        (* Some "apple pie" *) Fasta_record.seq record
      (* "ACTGactg" *)
    ]}

    {2 Example 2}

    If you have a fasta file something like this:

    {[
      >s1
      ACTG
      actg
    ]}

    Then you would get a [record] something like this:

    {[
      Fasta_record.id record (* "s1" *) Fasta_record.desc record (* None *)
        Fasta_record.seq record
      (* "ACTGactg" *)
    ]}

    {2 Example 3}

    To change a part of the [Fasta_record] use the [with_*] functions. E.g.,

    {[ Fasta_record.with_id "apple" record ]}

    would change give you a [t] with the [id] set to ["apple"]. *)
module Record : sig
  (** {1 API} *)

  type t [@@deriving sexp]

  val create : id:string -> desc:string option -> seq:string -> t
  (** [create ~id ~desc ~seq] creates a new [t]. Shouldn't raise as literally
      any values of the correct type are accepted. *)

  val of_header_exn : string -> t
  (** [of_header_exn header] returns a [t] from a FASTA header. May raise
      exceptions. Used internally for parsing FASTA files, but the code
      consuming the [bio_io] module probably won't need to use this function. *)

  val of_header : string -> t Or_error.t
  (** [of_header header] is like [of_header_exn header] except that it returns
      [Or_error.t] rather than raising exceptions. *)

  val to_string : t -> string
  (** [to_string t] returns a string representation of [t] ready to print to a
      FASTA output file. *)

  val to_string_nl : ?nl:string -> t -> string
  (** [to_string_nl t ~nl] returns a string representation of [t] ready to print
      to a FASTA output file, including a trailing newline (nl) string. [nl]
      defaults to ["\n"]. *)

  val serialize : t -> string
  (** [serialize t] returns the [Sexp] of [t] as a string. *)

  val equal : t -> t -> bool
  (** [equal this other] returns [true] if all fields of two [t]s are the same. *)

  val ( = ) : t -> t -> bool
  (** [Fasta_record.(this = other)] is like [equal this other]. *)

  val id : t -> string
  (** [id t] returns the [id] of the [t]. *)

  val desc : t -> string option
  (** [desc t] returns the [desc] (description) of the [t]. *)

  val seq : t -> string
  (** [seq t] returns the [seq] of the [t]. *)

  val seq_length : t -> int
  (** [seq_length t] returns the length of the [seq] of [t].

      If you construct a record by hand (e.g., with [create]), and there are
      spaces or other weird characters in the sequences, they will be counted in
      the length. E.g.,

      {[
        let r = Fasta_record.create ~id:"apple" ~desc:None ~seq:"a a" in
        assert (Int.(3 = Fasta_record.seq_length r))
      ]} *)

  val with_id : string -> t -> t
  (** [with_id new_id t] returns a [t] with [new_id] instead of the original
      [id]. *)

  val with_seq : string -> t -> t
  (** [with_seq new_seq t] returns a [t] with [new_seq] instead of the original
      [seq]. *)

  val with_desc : string option -> t -> t
  (** [with_desc new_desc t] returns a [t] with [new_desc] instead of the
      original [desc]. *)
end = struct
  type t = { id : string; desc : string option; seq : string } [@@deriving sexp]

  let create ~id ~desc ~seq = { id; desc; seq }

  let of_header_exn s =
    match String.is_prefix s ~prefix:">" with
    | false ->
        let msg =
          Printf.sprintf "Header line should start with '>'.  Got: '%s'"
            (String.prefix s 0)
        in
        failwith msg
    | true -> (
        match
          s |> String.strip
          |> String.chop_prefix_exn ~prefix:">"
          |> String.split ~on:' '
        with
        (* Empty header lines get id = "" *)
        | [ id ] -> { id; desc = None; seq = "" }
        | id :: desc ->
            { id; desc = Some (String.concat ~sep:" " desc); seq = "" }
        | [] ->
            (* String.split should at least give [""]. Should never get here. *)
            assert false)

  let of_header s =
    match of_header_exn s with
    | exception exn -> Or_error.error "Caught exception" exn Exn.sexp_of_t
    | result -> Or_error.return result

  let to_string r =
    match r.desc with
    | None -> Printf.sprintf ">%s\n%s" r.id r.seq
    | Some desc -> Printf.sprintf ">%s %s\n%s" r.id desc r.seq

  let to_string_nl ?(nl = "\n") r = to_string r ^ nl

  let serialize r = Sexp.to_string_hum (sexp_of_t r)

  let equal r1 r2 =
    let opt_equal = Option.equal String.equal in
    String.(r1.id = r2.id)
    && String.(r1.seq = r2.seq)
    && opt_equal r1.desc r2.desc

  let ( = ) = equal

  let id r = r.id
  let desc r = r.desc
  let seq r = r.seq
  let seq_length r = String.length r.seq

  let with_id id r = { r with id }
  let with_seq seq r = { r with seq }
  let with_desc desc r = { r with desc }
end

module In_channel : sig
  include Record_in_channel.S with type record := Record.t
end = struct
  module T = struct
    include Private.Peekable_in_channel
    type record = Record.t

    let clean_sequence s = String.filter s ~f:(fun c -> Char.(c <> ' '))

    let input_record_exn chan =
      let rec loop thing =
        match (peek_char chan, thing) with
        | None, None -> None
        | None, Some (header, seq) | Some '>', Some (header, seq) ->
            let r = Record.of_header_exn header in
            let seq = String.concat ~sep:"" @@ List.rev seq in
            Some (Record.with_seq seq r)
        | Some '>', None ->
            let line = input_line_exn ~fix_win_eol:true chan in
            loop (Some (line, []))
        | Some _, None ->
            failwith "Not at a header line, but not currently in a sequence"
        | Some _, Some (header, seq) ->
            let line = input_line_exn ~fix_win_eol:true chan in
            let seq_part = clean_sequence line in
            loop (Some (header, seq_part :: seq))
      in
      loop None
  end

  include T
  include Record_in_channel.Make (T)
end
