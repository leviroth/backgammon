open Base

type client_message =
  | Request_state
  | Move of (Location.t * int) list
[@@deriving sexp]

type server_message =
  | Unusable_dice of Color.t * (int * int)
  | Update_state of Game.t
  | Error_message of string
[@@deriving sexp]
