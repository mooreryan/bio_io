(** Functor and module types for creating record-based input channels *)

open! Base

(** An input channel for reading (biological) records.

    {1 Overview}

    - To get a list of records, use [records] or [with_file_records].
    - To iterate over records, use: [iter_records] or [with_file_iter_records].
    - To fold over records, use: [fold_records] or [with_file_fold_records].
    - To get a sequence of records, use: [record_sequence].

    {2 Examples}

    For usage examples, check out {{!Bio_io.Fasta.In_channel} Fasta.In_channel}
    and the other [In_channel] modules.

    {2 [with_file] functions}

    Functions that have the [with_file_] prefix in their name take a filename
    [string] rather than a [t]. They are a convenient way to avoid wrapping your
    all your code in [with_file] function calls.

    {2 Exceptions}

    Many functions will raise exceptions if one is encountered in the underlying
    I/O (Stdio), or if the format of the input file is bad. If you want to avoid
    exceptions, wrap the expression in [Base.Or_error.try_with]. *)
module type S = sig
  (** {1 API} *)

  type t
  type record

  val stdin : t
  (** [create file_name] opens an [t] on the standard input channel. *)

  val create : string -> t
  (** [create file_name] opens an input channel on the file specified by
      [file_name]. You may want to use [Base.Exn.protectx] with this. *)

  val close : t -> unit
  (** [close t] Close the [t]. *)

  val with_file : string -> f:(t -> 'a) -> 'a
  (** [with_file file_name ~f] executes [~f] on the channel created from
      [file_name] and ensures it is closed properly. *)

  val equal : t -> t -> bool
  (** [equal t1 t2] compares [t1] and [t2] for equality. *)

  val input_record : t -> record option
  (** [input_record t] returns [Some record] if there is a [record] to return.
      If there are no more records, [None] is returned. Raises exceptions on bad
      input (e.g., bad file format). *)

  (** {2 Folding over records} *)

  val fold_records : t -> init:'a -> f:('a -> record -> 'a) -> 'a
  (** [fold_records t ~init ~f] reduces all records from a [t] down to a single
      value of type ['a]. *)

  val foldi_records : t -> init:'a -> f:(int -> 'a -> record -> 'a) -> 'a
  (** [fold'_records t ~init ~f] is like {!fold_records} except that [f] is
      provided the 0-based record index as its first argument. *)

  (** {3 Folding with file name} *)

  val with_file_fold_records : string -> init:'a -> f:('a -> record -> 'a) -> 'a
  (** [with_file_fold_records file_name ~init ~f] is like
      [fold_records t ~init ~f] except that it is passed a file name, and it
      manages [t] automatically. See {!with_file}. *)

  val with_file_foldi_records :
    string -> init:'a -> f:(int -> 'a -> record -> 'a) -> 'a
  (** [with_file_foldi_records file_name ~init ~f] is like
      [foldi_records t ~init ~f] except that it is passed a file name, and it
      manages [t] automatically. See {!with_file}. *)

  (** {2 Iterating over records}

      The [iter] functions are like the [fold] functions except they do not take
      an [init] value and the [f] function returns [unit] insead of some other
      value ['a], and thus return [unit] rather than a value ['a].

      Use them for side-effects. *)

  val iter_records : t -> f:(record -> unit) -> unit
  (** [iter_records t ~f] calls [f] on each [record] in [t]. As [f] returns
      [unit] this is generally used for side effects. *)

  val iteri_records : t -> f:(int -> record -> unit) -> unit
  (** [iteri_records t ~f] is like [iteri_records t ~f] except that [f] is
      passed in the 0-indexed record index as its first argument. *)

  (** {3 Iterating with file name} *)

  val with_file_iter_records : string -> f:(record -> unit) -> unit
  (** [with_file_iter_records file_name ~init ~f] is like
      [iter_records t ~init ~f] except that it is passed a file name, and it
      manages [t] automatically. See {!with_file}. *)

  val with_file_iteri_records : string -> f:(int -> record -> unit) -> unit
  (** [with_file_iteri_records file_name ~init ~f] is like
      [iteri_records t ~init ~f] except that it is passed a file name, and it
      manages [t] automatically. See {!with_file}. *)

  (** {2 Getting records as a list}

      These functions return [record] lists. *)

  val records : t -> record list

  (** {3 With file name} *)

  val with_file_records : string -> record list

  (** {2 Getting records as a sequence}

      These are a bit different:

      * There are no [with_file] versions as you would have to do some fiddly
      things to keep the channel open, making them not so nice to use.

      * If an exception is raised sometime during the pipeline, it will blow up,
      but any successful processing that happended, will have happened. So be
      careful if you are doing side-effecting things. *)

  val record_sequence : t -> record Sequence.t
  (** [record_sequence t] returns a [Sequence.t] of [record]. *)
end

(** The basic in channel-like functions plus [input_record] function needed to
    make a [Record_in_channel] module. *)
module type In_channel_input_record = sig
  type t
  type record

  val equal : t -> t -> bool
  val stdin : t
  val close : t -> unit

  (** These two functions differ from the [Stdio.In_channel] in that they don't
      take a [binary] argument. *)

  val create : string -> t
  val with_file : string -> f:(t -> 'a) -> 'a
  val input_record : t -> record option
end

(** Functor for making [Record_in_channel] modules. *)
module Make (M : In_channel_input_record) :
  S with type t := M.t with type record := M.record = struct
  let equal = M.equal
  let stdin = M.stdin
  let create fname = M.create fname
  let close t = M.close t
  let with_file fname ~f = M.with_file fname ~f
  let input_record t = M.input_record t

  (* Folding over records *)

  let fold_records t ~init ~f =
    let rec loop acc = function
      | None -> acc
      | Some record' -> loop (f acc record') (input_record t)
    in
    loop init (input_record t)

  let foldi_records t ~init ~f =
    snd
      (fold_records t ~init:(0, init) ~f:(fun (i, acc) record ->
           (i + 1, f i acc record)))

  let with_file_fold_records fname ~init ~f =
    with_file fname ~f:(fun t -> fold_records t ~init ~f)

  let with_file_foldi_records fname ~init ~f =
    with_file fname ~f:(fun t -> foldi_records t ~init ~f)

  (* Iterating over records *)

  let iter_records t ~f = fold_records t ~init:() ~f:(fun () record -> f record)

  let iteri_records t ~f =
    foldi_records t ~init:() ~f:(fun i () record -> f i record)

  let with_file_iter_records fname ~f =
    with_file fname ~f:(fun t -> iter_records t ~f)

  let with_file_iteri_records fname ~f =
    with_file fname ~f:(fun t -> iteri_records t ~f)

  (* Returning a list of records *)

  let records t =
    List.rev
      (fold_records t ~init:[] ~f:(fun records record -> record :: records))

  let with_file_records fname = with_file fname ~f:records

  (* Sequence generating functions are a little bit different. *)

  let record_sequence t =
    Sequence.unfold ~init:t ~f:(fun ch ->
        Option.map (input_record ch) ~f:(fun record -> (record, ch)))
end
