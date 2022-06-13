open! Base

module Record : sig
  type t [@@deriving sexp]
  (** A btab query record. I.e., query name and a list of all [Btab.t] hits. *)

  val create : string -> Btab.Record.t list -> t
  val query : t -> string
  val hits : t -> Btab.Record.t list
end = struct
  type t = { query : string; hits : Btab.Record.t list } [@@deriving sexp]

  let create query hits = { query; hits }
  let query t = t.query
  let hits t = t.hits
end

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
            loop (Some r.query) (r :: hits)
        | Some last_query', None -> mkrecord last_query' hits
        | Some last_query', Some line ->
            (* We need to check if this is a new record or not. *)
            let r = Btab.Record.of_string line in
            if is_new_query ~last_query:last_query' ~new_query:r.query then
              mkrecord last_query' hits
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
