(** Parsing btab files. Treats query as the record unit rather than individual
    hits. *)

open! Base

(** A record type for Btab homology search files

    {1 Overview}

    Unlike the {!Btab} module, the query sequence is the basis for the record.

    All hits are grouped by the query sequence. For example, in a btab file with
    query-target pairs, [q1-t1, q1-t2, q2-t2] then you will have two records.
    One for [q1] and one for [q2]. *)
module Record : sig
  (** {1 API} *)

  type t
  (** A btab query record. I.e., query name and a list of all [Btab.t] hits. *)

  (** {2 Creating} *)

  val create : string -> Btab.Record.t list -> t
  (** [create query_name hit_list] creates a [t] given the name of the query and
      a list of hits. You probably won't use this function directly. *)

  (** {2 Accessing} *)

  val query : t -> string
  (** [query t] returns the name of the query for this record (i.e., for the
      list of hits). *)

  val hits : t -> Btab.Record.t list
  (** [hits t] returns the list of hits associated with this query sequence. *)
end = struct
  type t = { query : string; hits : Btab.Record.t list }

  let create query hits = { query; hits }
  let query t = t.query
  let hits t = t.hits
end

(** [In_channel] for Btab files where each query in the file is a single record.

    {1 Overview}

    {b WARNING}: This module assumes that queries are sorted. One case in which
    this assumption does not hold is with [mmseqs] when using more than one
    iteration. E.g., [mmseqs easy-search --num-iterations 3]. This behavior will
    likely change in the future.

    You should consider this module experimental.

    {1 Example}

    Here is a short example program. It reads a btab file and prints out the
    records.

    {[
      open! Base
      open! Bio_io.Btab_queries

      let parse_argv () =
        match Sys.get_argv () with
        | [| _; file_name |] -> file_name
        | _ -> failwith "missing file_name"

      let () =
        let file_name = parse_argv () in
        In_channel.with_file_iter_records_exn file_name ~f:(fun r ->
            Stdio.print_endline "===";
            Stdio.print_endline @@ Record.query r;
            let hits = List.map ~f:Bio_io.Btab.Record.parse @@ Record.hits r in
            Stdio.print_s @@ [%sexp_of: Bio_io.Btab.Record.Parsed.t list] hits)
    ]}

    The output will be somthing like.

    {[
      ===
      Q 1
      (((query "Q 1") (target q1t1) (pident 0.1) (alnlen 2) (mismatch 3)
        (gapopen 4) (qstart 5) (qend 6) (tstart 7) (tend 8) (evalue 9.99E-05)
        (bits 10) (qlen ()) (tlen ()))
       ((query "Q 1") (target q1t2) (pident 0.11) (alnlen 12) (mismatch 13)
        (gapopen 14) (qstart 15) (qend 16) (tstart 17) (tend 18) (evalue 1.9E-05)
        (bits 20) (qlen ()) (tlen ())))
      ===
      Q_2
      (((query Q_2) (target q2t1) (pident 0.21) (alnlen 22) (mismatch 23)
        (gapopen 24) (qstart 25) (qend 26) (tstart 27) (tend 28) (evalue 2.9E-05)
        (bits 30) (qlen ()) (tlen ())))
    ]} *)
module In_channel = struct
  module T = struct
    include Private.Peekable_in_channel

    type record = Record.t

    (* TODO put somewhere in the docs that this assumes that the queries are
       sorted. One specific case in which they are not is with mmseqs searches
       with multiple iterations. *)
    let input_record_exn ic =
      let consume_line ic =
        Btab.Record.of_string @@ input_line_exn ~fix_win_eol:true ic
      in
      let mkrecord query hits = Some (Record.create query (List.rev hits)) in
      let is_new_query ~last_query ~new_query =
        String.(last_query <> new_query)
      in
      let rec loop last_query hits =
        match (last_query, peek_line ~fix_win_eol:true ic) with
        | None, None -> None
        | None, Some _ ->
            (* Need to consume this line right away. *)
            let r = consume_line ic in
            loop (Some (Btab.Record.query r)) (r :: hits)
        | Some last_query', None -> mkrecord last_query' hits
        | Some last_query', Some line ->
            (* We need to check if this is a new record or not. *)
            let r = Btab.Record.of_string line in
            if
              is_new_query ~last_query:last_query'
                ~new_query:(Btab.Record.query r)
            then mkrecord last_query' hits
            else
              (* Consume this line and loop, because we have more hits for this
                 current query. *)
              let r = consume_line ic in
              loop last_query (r :: hits)
      in
      loop None []
  end

  include T
  include Record_in_channel.Make (T)
end
