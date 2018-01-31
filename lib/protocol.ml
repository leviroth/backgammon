open Base

type client_message =
  | Request_color of Color.t
  | Request_state
  | Move of Color.t * int * (Location.t * int) list
[@@deriving sexp]

type server_message =
  | Send_color_secret of Color.t * int
  | Unusable_dice of Color.t * (int * int)
  | Update_state of Game.t
  | Error_message of string
[@@deriving sexp]
