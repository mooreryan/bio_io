open! Base

(** An input channel for reading (biological) records.

    {1 Overview}

    {2 Quick start}

    - To get a list of records, use [records] or [with_file_records].
    - To get a sequence of records, use: [record_sequence].
    - To fold over records, use: [fold_records] or [with_file_fold_records].
    - To iterate over records, use: [iter_records] or [with_file_iter_records].

    For some more info, check out the usage examples.

    {2 Function flavors}

    Most functions in this module come in a few different flavors.

    - Functions with [with_file_] prefix
    - Functions with the [_exn] suffix
    - Functions that return [Or_error.t]

    Functions that have the [with_file_] prefix in their name take a filename
    [string] rather than a [t]. They are a convenient way to avoid wrapping your
    all your code in [with_file] function calls.

    Functions that the [_exn] suffix in their name may raise exceptions, either
    from logic specific to this module, or from calls to other [Core_kernel]
    functions.

    Functions that return [Or_error.t] catch {i all} exceptions, even those not
    originating in the logic of this module. These functions shouldn't raise in
    practice. In other words, if you aren't using a function with [_exn] in the
    name, and you get an exception, then you have found a bug.

    Here is an example of the different "flavors" of [fold_records]:

    - [fold_records]: the normal [t] accepting, [Or_error.t] returning flavor
    - [fold_records_exn]: takes [t] but may raise exceptions instead of
      returning [Or_error.t]
    - [with_file_fold_records]: takes a file name ([string]) rather than a [t],
      and returns [Or_error.t]
    - [with_file_fold_records_exn]: takes a file name ([string]) rather than a
      [t], and may raise exceptions *)
module type S = sig
  (** {1 API} *)

  type t

  type record

  val stdin : t
  (** [create_exn file_name] opens an [t] on the standard input channel. *)

  val create_exn : string -> t
  (** [create_exn file_name] opens an input channel on the file specified by
      [file_name]. *)

  val create : string -> t Or_error.t
  (** [create file_name] opens an input channel on the file specified by
      [file_name]. *)

  val close_exn : t -> unit
  (** [close_exn t] Close the [t]. Raises if the call fails. *)

  val close : t -> unit Or_error.t
  (** [close t] is like [close_exnt t] except that it shouldn't raise. *)

  val with_file_exn : string -> f:(t -> 'a) -> 'a
  (** [with_file_exn file_name ~f] executes [~f] on the channel created from
      [file_name] and closes it afterwards. *)

  val with_file : string -> f:(t -> 'a) -> 'a Or_error.t
  (** [with_file file_name ~f] is like [with_file_exn file_name ~f] except that
      it shouldn't raise. *)

  val equal : t -> t -> bool
  (** [equal t1 t2] compares [t1] and [t2] for equality. *)

  val input_record_exn : t -> record option
  (** [input_record_exn t] returns [Some record] if there is a [record] to
      return. If there are no more records, [None] is returned. [Exn] is raised
      on bad input. *)

  val input_record : t -> record option Or_error.t
  (** [input_record t] is like [input_record_exn t] except that it should not
      raise exceptions. *)

  (** {2 Folding over records} *)

  val fold_records_exn : t -> init:'a -> f:('a -> record -> 'a) -> 'a
  (** [fold_records_exn t ~init ~f] reduces all records from a [t] down to a
      single value of type ['a]. *)

  val fold_records : t -> init:'a -> f:('a -> record -> 'a) -> 'a Or_error.t
  (** [fold_records t ~init ~f] is like [fold_records_exn t ~init ~f] except
      that it should not raise exceptions. Rather than deal with exceptions
      inside the reducing function, you must deal with them at the end when
      handling the return value. *)

  val foldi_records_exn : t -> init:'a -> f:(int -> 'a -> record -> 'a) -> 'a
  (** Like [fold_records_exn t ~init ~f] except that [f] is provided the 0-based
      record index as its first argument. See {!fold_records_exn}.*)

  val foldi_records :
    t -> init:'a -> f:(int -> 'a -> record -> 'a) -> 'a Or_error.t
  (** Like [foldi_records_exn t ~init ~f] except that it shouldn't raise. See
      {!foldi_records_exn}. *)

  (** {3 Folding with file name} *)

  val with_file_fold_records_exn :
    string -> init:'a -> f:('a -> record -> 'a) -> 'a
  (** [with_file_fold_records_exn file_name ~init ~f] is like
      [fold_records_exn t ~init ~f] except that it is passed a file name, and it
      manages [t] automatically. See {!with_file}. *)

  val with_file_fold_records :
    string -> init:'a -> f:('a -> record -> 'a) -> 'a Or_error.t
  (** [with_file_fold_records file_name ~init ~f] is like
      [fold_records t ~init ~f] except that it is passed a file name, and it
      manages [t] automatically. See {!with_file}. *)

  val with_file_foldi_records_exn :
    string -> init:'a -> f:(int -> 'a -> record -> 'a) -> 'a
  (** [with_file_foldi_records_exn file_name ~init ~f] is like
      [foldi_records_exn t ~init ~f] except that it is passed a file name, and
      it manages [t] automatically. See {!with_file}. *)

  val with_file_foldi_records :
    string -> init:'a -> f:(int -> 'a -> record -> 'a) -> 'a Or_error.t
  (** [with_file_foldi_records file_name ~init ~f] is like
      [fold'_records t ~init ~f] except that it is passed a file name, and it
      manages [t] automatically. See {!with_file}. *)

  (** {2 Iterating over records}

      The [iter] functions are like the [fold] functions except they do not take
      an [init] value and the [f] function returns [unit] insead of some other
      value ['a], and thus return [unit] rather than a value ['a].

      They are mainly called for side effects. *)

  val iter_records_exn : t -> f:(record -> unit) -> unit
  (** [iter_records_exn t ~f] calls [f] on each [record] in [t]. As [f] returns
      [unit] this is generally used for side effects. *)

  val iter_records : t -> f:(record -> unit) -> unit Or_error.t
  (** [iter_records t ~f] is like [iter_records_exn t ~f] except that it
      shouldn't raise. *)

  val iteri_records_exn : t -> f:(int -> record -> unit) -> unit
  (** [iteri_records_exn t ~f] is like [iteri_records_exn t ~f] except that [f]
      is passed in the 0-indexed record index as its first argument. *)

  val iteri_records : t -> f:(int -> record -> unit) -> unit Or_error.t
  (** [iteri_records t ~f] is like [iteri_records_exn t ~f] except that it
      shouldn't raise. *)

  (** {3 Iterating with file name} *)

  val with_file_iter_records_exn : string -> f:(record -> unit) -> unit
  (** [with_file_iter_records_exn file_name ~init ~f] is like
      [iter_records_exn t ~init ~f] except that it is passed a file name, and it
      manages [t] automatically. See {!with_file}. *)

  val with_file_iter_records : string -> f:(record -> unit) -> unit Or_error.t
  (** [with_file_iter_records file_name ~init ~f] is like
      [iter_records t ~init ~f] except that it is passed a file name, and it
      manages [t] automatically. See {!with_file}. *)

  val with_file_iteri_records_exn : string -> f:(int -> record -> unit) -> unit
  (** [with_file_iteri_records_exn file_name ~init ~f] is like
      [iteri_records_exn t ~init ~f] except that it is passed a file name, and
      it manages [t] automatically. See {!with_file}. *)

  val with_file_iteri_records :
    string -> f:(int -> record -> unit) -> unit Or_error.t
  (** [with_file_iteri_records file_name ~init ~f] is like
      [iteri_records t ~init ~f] except that it is passed a file name, and it
      manages [t] automatically. See {!with_file}. *)

  (** {2 Getting records as a list}

      These functions return lists of [records]s. *)

  val records_exn : t -> record List.t

  val records : t -> record List.t Or_error.t

  (** {3 With file name} *)

  val with_file_records_exn : string -> record List.t

  val with_file_records : string -> record List.t Or_error.t

  (** {2 Getting records as a sequence}

      These are a bit different:

      * There are no [with_file] versions as you would have to do some fiddly
      things to keep the channel open, making them not so nice to use.

      * Each [record] that is yielded is wrapped in an [Or_error.t]. This is
      different from the [iter], [fold], and other non [_exn] functions in which
      case the entire result is wrapped in an [Or_error.t], letting you ignore
      errors in the passed in [~f] function and deal with failure once. *)

  val record_sequence_exn : t -> record Sequence.t
  (** [record_sequence_exn t] returns a [Sequence.t] of [record]. May raise
      exceptions. *)

  val record_sequence : t -> record Or_error.t Sequence.t
  (** [record_sequence t] is like [record_sequence_exn t] except that instead of
      raising exceptions, each item of the sequence is a [record Or_error.t]
      rather than an "unwrapped" [record]. This could make things annoying to
      deal with. If you don't want exceptions, you could instead wrap your
      entire sequence processing pipeline in a call to [with_file] and handle
      the [Or_error.t] in that way. See the pipelines usage examples for more
      info. *)
end

(** The basic in channel-like functions plus input_record function needed to
    make a [Record_in_channel] module. *)
module type In_channel_input_record = sig
  type t

  type record

  val equal : t -> t -> bool

  val stdin : t

  val close : t -> unit

  (** These two functions differ from the Stdio.In_channel in that they don't
      take a [binary] argument. *)

  val create : string -> t

  val with_file : string -> f:(t -> 'a) -> 'a

  val input_record_exn : t -> record option
end

(** Functor for making [Record_in_channel] modules. *)
module Make (M : In_channel_input_record) :
  S with type t := M.t with type record := M.record = struct
  let equal = M.equal

  let stdin = M.stdin

  let create_exn fname = M.create fname

  let create fname = Utils.try1 create_exn fname

  let close_exn t = M.close t

  let close t = Utils.try1 close_exn t

  let with_file_exn fname ~f = M.with_file fname ~f

  let with_file fname ~f = Utils.try_map with_file_exn fname ~f

  let input_record_exn t = M.input_record_exn t

  let input_record t = Utils.try1 input_record_exn t

  (* Folding over records *)

  let fold_records_exn t ~init ~f =
    let rec loop acc = function
      | None -> acc
      | Some record' -> loop (f acc record') (input_record_exn t)
    in
    loop init (input_record_exn t)

  let fold_records t ~init ~f = Utils.try_fold fold_records_exn t ~init ~f

  let foldi_records_exn t ~init ~f =
    snd
      (fold_records_exn t ~init:(0, init) ~f:(fun (i, acc) record ->
           (i + 1, f i acc record)))

  let foldi_records t ~init ~f = Utils.try_fold foldi_records_exn t ~init ~f

  let with_file_fold_records_exn fname ~init ~f =
    with_file_exn fname ~f:(fun t -> fold_records_exn t ~init ~f)

  let with_file_fold_records fname ~init ~f =
    Utils.try_fold with_file_fold_records_exn fname ~init ~f

  let with_file_foldi_records_exn fname ~init ~f =
    with_file_exn fname ~f:(fun t -> foldi_records_exn t ~init ~f)

  let with_file_foldi_records fname ~init ~f =
    Utils.try_fold with_file_foldi_records_exn fname ~init ~f

  (* Iterating over records *)

  let iter_records_exn t ~f =
    fold_records_exn t ~init:() ~f:(fun () record -> f record)

  let iter_records t ~f = fold_records t ~init:() ~f:(fun () record -> f record)

  let iteri_records_exn t ~f =
    foldi_records_exn t ~init:() ~f:(fun i () record -> f i record)

  let iteri_records t ~f = Utils.try_map iteri_records_exn t ~f

  let with_file_iter_records_exn fname ~f =
    with_file_exn fname ~f:(fun t -> iter_records_exn t ~f)

  let with_file_iter_records fname ~f =
    Utils.try_map with_file_iter_records_exn fname ~f

  let with_file_iteri_records_exn fname ~f =
    with_file_exn fname ~f:(fun t -> iteri_records_exn t ~f)

  let with_file_iteri_records fname ~f =
    Utils.try_map with_file_iteri_records_exn fname ~f

  (* Returning a list of records *)

  let records_exn t =
    List.rev
      (fold_records_exn t ~init:[] ~f:(fun records record -> record :: records))

  let records t = Utils.try1 records_exn t

  let with_file_records_exn fname = with_file_exn fname ~f:records_exn

  let with_file_records fname = Utils.try1 with_file_records_exn fname

  (* Sequence generating functions are a little bit different. *)

  let record_sequence_exn t =
    Sequence.unfold ~init:t ~f:(fun ch ->
        Option.map (input_record_exn ch) ~f:(fun record -> (record, ch)))

  let record_sequence t =
    Sequence.unfold ~init:(Some t) ~f:(fun t' ->
        match t' with
        (* None means the sequence is over. *)
        | None -> None
        | Some t'' -> (
            match input_record t'' with
            (* Some Error seems weird, but we need to yield an Error so the
               caller can handle it, then we need to trigger one more yield
               iteration to end the sequence next time with the None channel. *)
            | Error err -> Some (Error err, None)
            | Ok record -> (
                match record with
                (* None needed here to end the Sequence. *)
                | None -> None
                | Some record' -> Some (Or_error.return record', Some t''))))
end
