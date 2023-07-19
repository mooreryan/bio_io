(** Mishmosh of internal helper functions *)

open! Base

let try0 f =
  match f () with
  | exception exn ->
      Or_error.error "Caught exception" exn Exn.sexp_of_t
  | result ->
      Or_error.return result

let try1 f a =
  match f a with
  | exception exn ->
      Or_error.error "Caught exception" exn Exn.sexp_of_t
  | result ->
      Or_error.return result

let try1' ~msg f a =
  match f a with
  | exception exn ->
      Or_error.error msg exn Exn.sexp_of_t
  | result ->
      Or_error.return result

(* Complement dna:
   https://arep.med.harvard.edu/labgc/adnan/projects/Utilities/revcomp.html.contents

   Note that S->S, W->W, and N->N, but they are included here for clarity.*)

let complement' =
  String.tr_multi ~target:"AaTtUuGgCcYyRrSsWwKkMmBbDdHhVvNn"
    ~replacement:"TtAaAaCcGgRrYySsWwMmKkVvHhDdBbNn"

let complement s =
  let tr = Staged.unstage complement' in
  tr s

let rev_complement s = String.rev @@ complement s
