open Base

type t =
  | Request_state
  | Move of (Location.t * int) list
[@@deriving sexp]
