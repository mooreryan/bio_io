(** Parsing FASTQ files *)

open! Base

(** A record type for FASTQ files.

    {1 Overview}

    Very similar to the {!Fasta.Record.t}. See that module for more
    details.contents

    {2 Fields}

    For the "header" line, the [id] is everything up to the first space, and the
    [desc] is everything after. Note, that [desc] is optional. E.g.,

    {[
      @this_is_the_id this is the desc
       ^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^
       id ----------| Some desc -----|
    ]}

    The other possibly weird field is [extra]. It is simply everything on the
    line following the [+] (i.e., the third line of the 4 line FASTQ file).

    It is currently {i NOT} a parse error if the sequence length and quality
    string length are different lengths. This may change in the future. *)
module Record : sig
  (** {1 API} *)

  type t [@@deriving sexp]

  val create :
    id:string ->
    desc:string option ->
    seq:string ->
    qual:string ->
    extra:string option ->
    t
  (** [create ~id ~desc ~seq ~qual ~extra] creates a new [t]. Shouldn't raise as
      literally any values of the correct type are accepted. *)

  val to_string : t -> string
  (** [to_string t] returns a string representation of [t] ready to print to a
      FASTQ output file. *)

  val to_string_nl : ?nl:string -> t -> string
  (** [to_string_nl t ~nl] returns a string representation of [t] ready to print
      to a FASTQ output file, including a trailing newline (nl) string. [nl]
      defaults to ["\n"]. *)

  val equal : t -> t -> bool
  (** [equal this other] returns [true] if all fields of two [t]s are the same. *)

  val ( = ) : t -> t -> bool
  (** [Fastq.Record.(this = other)] is like [equal this other]. *)

  val id : t -> string
  (** [id t] returns the [id] of the [t]. *)

  val desc : t -> string option
  (** [desc t] returns the [desc] (description) of the [t]. *)

  val seq : t -> string
  (** [seq t] returns the [seq] of the [t]. *)

  val qual : t -> string
  (** [qual t] returns the [qual] of [t]. *)

  val extra : t -> string option
  (** [extra t] returns the [extra] of the [t] if there is one. It is whatever
      is after the [+] line. Technically it should be the same ID+desc found in
      the header line, but this library just treats it as a blob of data. *)

  val seq_length : t -> int
  (** [seq_length t] returns the length of the [seq] of [t].

      If you construct a record by hand (e.g., with [create]), and there are
      spaces or other weird characters in the sequences, they will be counted in
      the length. E.g.,

      {[
        let r =
          Fastq.Record.create ~id:"apple" ~desc:None ~seq:"a a" ~qual:". ."
            ~extra:None
        in
        assert (Int.(3 = Fastq.Record.seq_length r))
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

  val with_qual : string -> t -> t
  (** [with_qual new_qual t] returns a [t] with [new_qual] instead of the
      original [qual]. *)

  val with_extra : string option -> t -> t
  (** [with_extra new_extra t] returns a [t] with [new_extra] instead of the
      original [extra]. *)

  val rev : t -> t
  (** [rev t] returns the reverse of [t]. I.e., the [seq] and the [qual] are
      reversed. *)

  val comp : t -> t
  (** [comp t] returns the complement of [t]. I.e., the [seq] is complemented.
      Uses IUPAC conventions. Any "base" (char) that isn't part of the IUPAC
      passes through unchanged. Note that [comp] does not round-trip. *)

  val rev_comp : t -> t
  (** [rev_comp t] returns the reverse complement of [t]. I.e., the [seq] is
      reverse complemented, and the quality score is reversed to match it. Uses
      IUPAC conventions. Any "base" (char) that isn't part of the IUPAC passes
      through unchanged. Note that [rev_comp] does not round-trip. *)
end = struct
  type t = {
    id : string;
    desc : string option;
    seq : string;
    qual : string;
    extra : string option;
  }
  [@@deriving sexp]

  let create ~id ~desc ~seq ~qual ~extra = { id; desc; seq; qual; extra }

  let to_string r =
    let header =
      match r.desc with None -> r.id | Some desc -> r.id ^ " " ^ desc
    in
    let extra = Option.value ~default:"" r.extra in
    Printf.sprintf "@%s\n%s\n+%s\n%s" header r.seq extra r.qual

  let to_string_nl ?(nl = "\n") r = to_string r ^ nl

  let equal r1 r2 =
    String.(r1.id = r2.id)
    && String.(r1.seq = r2.seq)
    && String.(r1.qual = r2.qual)
    && Option.equal String.equal r1.desc r2.desc
    && Option.equal String.equal r1.extra r2.extra

  let ( = ) = equal
  let id r = r.id
  let desc r = r.desc
  let seq r = r.seq
  let qual r = r.qual
  let extra r = r.extra
  let seq_length r = String.length r.seq
  let with_id id r = { r with id }
  let with_seq seq r = { r with seq }
  let with_desc desc r = { r with desc }
  let with_qual qual r = { r with qual }
  let with_extra extra r = { r with extra }
  let rev t = t |> with_seq (String.rev t.seq) |> with_qual (String.rev t.qual)
  let comp t = t |> with_seq (Utils.complement t.seq)

  let rev_comp t =
    t |> with_seq (Utils.rev_complement t.seq) |> with_qual (String.rev t.qual)
end

(** [In_channel] for FASTQ records.

    For more general info, see the {!Record_in_channel} module mli file. For
    more specific info, see the {!Fasta.In_channel} module.

    Note: Empty fastq files parse OK, but are empty.

    {1:examples Examples}

    These examples basically match those in the {!Fasta.In_channel} module.

    {2 Return all records in a list}

    {[
      let records = Fastq.In_channel.with_file_records fname
    ]}

    {2 Iterating over records}

    Use the [iter] functions when you need to go over each record and perform
    some side-effects with them.

    Print sequence IDs and sequence lengths

    {[
      let () =
        Fastq.In_channel.with_file_iter_records "sequences.fastq"
          ~f:(fun record ->
            let open Fastq.Record in
            printf "%s => %d\n" (id record) (seq_length record))
    ]}

    Print sequence index, IDs, and sequence lengths.

    This is like the last example except that we also want to print the index.
    The first record is 0, the 2nd is 1, etc.

    {[
      let () =
        Fastq.In_channel.with_file_iteri_records "sequences.fastq"
          ~f:(fun i record ->
            let open Fastq.Record in
            printf "%d: %s => %d\n" (i + 1) (id record) (seq_length record))
    ]}

    {2 Folding over records}

    If you need to reduce all the records down to a single value, use the [fold]
    functions.

    Get total length of all sequences in the file.

    {[
      let total_length =
        Fastq.In_channel.with_file_fold_records "sequences.fastq" ~init:0
          ~f:(fun length record -> length + Fastq.Record.seq_length record)
    ]}

    {2:pipelines Pipelines with records}

    Sometimes you have a "pipeline" of computations that you need to do one
    after the other on records. In that case, you could the [sequence]
    functions. Here's a silly example.

    {[
      let () =
        Fastq.In_channel.with_file name ~f:(fun chan ->
            Fastq.In_channel.record_sequence chan
            (* Add sequence index to record description *)
            |> Sequence.mapi ~f:(fun i record ->
                   let new_desc =
                     match Fastq.Record.desc record with
                     | None -> Some (sprintf "sequence %d" i)
                     | Some old_desc ->
                         Some (sprintf "%s -- sequence %d" old_desc i)
                   in
                   Fastq.Record.with_desc new_desc record)
            (* Convert all sequence chars to lowercase *)
            |> Sequence.map ~f:(fun record ->
                   let new_seq = String.lowercase (Fastq.Record.seq record) in
                   Fastq.Record.with_seq new_seq record)
            (* Print sequences *)
            |> Sequence.iter ~f:(fun record ->
                   print_endline @@ Fastq.Record.to_string record))
    ]}

    One thing to watch out for though...if you get an exception half way through
    and you are running side-effecting code like we are here then part of your
    side effects will have occured and part of them will {i not} have occured.

    As you can see, if that fastq file has more than one sequence it will hit
    the [assert false] and blow up. *)
module In_channel : sig
  include Record_in_channel.S with type record := Record.t
end = struct
  module T = struct
    include Private.Peekable_in_channel

    type record = Record.t

    let clean_sequence s = String.filter s ~f:(fun c -> Char.(c <> ' '))

    (* Be sure not to call this on a quality line as it may start with [@]. *)

    let parse_header_line line =
      if not (String.is_prefix line ~prefix:"@") then
        failwith
        @@ Printf.sprintf
             "I expected to see a header line starting with '@', but saw '%s' \
              instead."
        @@ String.prefix line 0;

      match
        String.split ~on:' '
        @@ String.chop_prefix_exn ~prefix:"@"
        @@ String.strip line
      with
      | [ id ] -> (id, None)
      | id :: desc ->
          (id, Some (String.concat ~sep:" " desc))
          (* String.split should at least give [""]. Should never get here. *)
      | [] -> assert false

    let parse_extra_line line =
      match String.chop_prefix line ~prefix:"+" with
      | Some s -> s
      | None ->
          failwith
          @@ Printf.sprintf
               "I expected to see an extra line starting with '+', but saw \
                '%s' instead."
          @@ String.prefix line 0

    let input_record chan =
      let rec loop ?header_line ?seq_line ?extra_line i =
        match (input_line ~fix_win_eol:true chan, i) with
        | Some line, 0 -> loop (i + 1) ~header_line:line
        | Some line, 1 -> loop (i + 1) ?header_line ~seq_line:line
        | Some line, 2 -> loop (i + 1) ?header_line ?seq_line ~extra_line:line
        | Some line, 3 ->
            let id, desc =
              parse_header_line @@ Option.value_exn @@ header_line
            in
            let seq = clean_sequence @@ Option.value_exn @@ seq_line in
            let extra = Option.map ~f:parse_extra_line extra_line in
            let qual = clean_sequence line in
            Some (Record.create ~id ~desc ~seq ~extra ~qual)
        | None, 0 -> None
        | None, 1 | None, 2 | None, 3 ->
            failwith
              "I was partway through a record, but I hit the end of the file."
        | Some _, _ | None, _ -> assert false
      in

      loop 0
  end

  include T
  include Record_in_channel.Make (T)
end
