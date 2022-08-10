(** Parsing FASTA files *)

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
      (* "s1" *)
      Fasta.Record.id record;;

      (* Some "apple pie" *)
      Fasta.Record.desc record;;

      (* "ACTGactg" *)
      Fasta.Record.seq record
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
      (* "s1" *)
      Fasta.Record.id record;;

      (* None *)
      Fasta.Record.desc record;;

      (* "ACTGactg" *)
      Fasta.Record.seq record
    ]}

    {2 Example 3}

    To change a part of the [Fasta.Record] use the [with_*] functions. E.g.,

    {[
      Fasta.Record.with_id "apple" record
    ]}

    would change give you a [t] with the [id] set to ["apple"]. *)
module Record : sig
  (** {1 API} *)

  type t [@@deriving sexp]

  val create : id:string -> desc:string option -> seq:string -> t
  (** [create ~id ~desc ~seq] creates a new [t]. Shouldn't raise as literally
      any values of the correct type are accepted. *)

  val to_string : t -> string
  (** [to_string t] returns a string representation of [t] ready to print to a
      FASTA output file. *)

  val to_string_nl : ?nl:string -> t -> string
  (** [to_string_nl t ~nl] returns a string representation of [t] ready to print
      to a FASTA output file, including a trailing newline (nl) string. [nl]
      defaults to ["\n"]. *)

  val equal : t -> t -> bool
  (** [equal this other] returns [true] if all fields of two [t]s are the same. *)

  val ( = ) : t -> t -> bool
  (** [Fasta.Record.(this = other)] is like [equal this other]. *)

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
        let r = Fasta.Record.create ~id:"apple" ~desc:None ~seq:"a a" in
        assert (Int.(3 = Fasta.Record.seq_length r))
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

  let to_string r =
    match r.desc with
    | None -> Printf.sprintf ">%s\n%s" r.id r.seq
    | Some desc -> Printf.sprintf ">%s %s\n%s" r.id desc r.seq

  let to_string_nl ?(nl = "\n") r = to_string r ^ nl

  let equal r1 r2 =
    String.(r1.id = r2.id)
    && String.(r1.seq = r2.seq)
    && Option.equal String.equal r1.desc r2.desc

  let ( = ) = equal
  let id r = r.id
  let desc r = r.desc
  let seq r = r.seq
  let seq_length r = String.length r.seq
  let with_id id r = { r with id }
  let with_seq seq r = { r with seq }
  let with_desc desc r = { r with desc }
end

(** [In_channel] for FASTA records. For more general info, see the
    [Record_in_channel] module mli file.

    {1:examples Examples}

    {2 Return all records in a list}

    {[
      let records = Fasta.In_channel.with_file_records fname
    ]}

    {2 Iterating over records}

    Use the [iter] functions when you need to go over each record and perform
    some side-effects with them.

    Print sequence IDs and sequence lengths

    {[
      let () =
        Fasta.In_channel.with_file_iter_records "sequences.fasta"
          ~f:(fun record ->
            let open Fasta.Record in
            printf "%s => %d\n" (id record) (seq_length record))
    ]}

    Print sequence index, IDs, and sequence lengths.

    This is like the last example except that we also want to print the index.
    The first record is 0, the 2nd is 1, etc.

    {[
      let () =
        Fasta.In_channel.with_file_iteri_records "sequences.fasta"
          ~f:(fun i record ->
            let open Fasta.Record in
            printf "%d: %s => %d\n" (i + 1) (id record) (seq_length record))
    ]}

    {2 Folding over records}

    If you need to reduce all the records down to a single value, use the [fold]
    functions.

    Get total length of all sequences in the file.

    {[
      let total_length =
        Fasta.In_channel.with_file_fold_records "sequences.fasta" ~init:0
          ~f:(fun length record -> length + Fasta.Record.seq_length record)
    ]}

    {2:pipelines Pipelines with records}

    Sometimes you have a "pipeline" of computations that you need to do one
    after the other on records. In that case, you could the [sequence]
    functions. Here's a silly example.

    {[
      let () =
        Fasta.In_channel.with_file name ~f:(fun chan ->
            Fasta.In_channel.record_sequence chan
            (* Add sequence index to record description *)
            |> Sequence.mapi ~f:(fun i record ->
                   let new_desc =
                     match Fasta.Record.desc record with
                     | None -> Some (sprintf "sequence %d" i)
                     | Some old_desc ->
                         Some (sprintf "%s -- sequence %d" old_desc i)
                   in
                   Fasta.Record.with_desc new_desc record)
            (* Convert all sequence chars to lowercase *)
            |> Sequence.map ~f:(fun record ->
                   let new_seq = String.lowercase (Fasta.Record.seq record) in
                   Fasta.Record.with_seq new_seq record)
            (* Print sequences *)
            |> Sequence.iter ~f:(fun record ->
                   print_endline @@ Fasta.Record.to_string record))
    ]}

    One thing to watch out for though...if you get an exception half way through
    and you are running side-effecting code like we are here then part of your
    side effects will have occured and part of them will {i not} have occured.

    As you can see, if that fasta file has more than one sequence it will hit
    the [assert false] and blow up. *)
module In_channel : sig
  include Record_in_channel.S with type record := Record.t
end = struct
  module T = struct
    include Private.Peekable_in_channel

    type record = Record.t

    let clean_sequence s = String.filter s ~f:(fun c -> Char.(c <> ' '))

    let parse_header_line line =
      assert (String.is_prefix line ~prefix:">");
      match
        String.split ~on:' '
        @@ String.chop_prefix_exn ~prefix:">"
        @@ String.strip line
      with
      (* Empty header lines get id = "" *)
      | [ id ] -> Record.create ~id ~desc:None ~seq:""
      | id :: desc ->
          Record.create ~id ~desc:(Some (String.concat ~sep:" " desc)) ~seq:""
      | [] ->
          (* String.split should at least give [""]. Should never get here. *)
          assert false

    let input_record chan =
      let rec loop thing =
        match (peek_char chan, thing) with
        | None, None -> None
        | None, Some (header, seq) | Some '>', Some (header, seq) ->
            let r = parse_header_line header in
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
