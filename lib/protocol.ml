open Core_kernel
open Async_rpc_kernel.Rpc

type move =
  { color : Color.t
  ; secret : int
  ; sequence : (Location.t * int) list
  }
[@@deriving bin_io]

type broadcast_message =
  | Unusable_dice of Color.t * (int * int)
  | Update_state of Game.t
[@@deriving sexp, bin_io]

let version = 0

let move_rpc =
  Rpc.create
    ~name:"move"
    ~version
    ~bin_query:bin_move
    ~bin_response:(Result.bin_t Unit.bin_t String.bin_t)

let request_state_rpc =
  Pipe_rpc.create
    ~name:"request_state"
    ~version
    ~bin_query:Unit.bin_t
    ~bin_response:bin_broadcast_message
    ~bin_error:Unit.bin_t
    ()

let request_color_rpc =
  Rpc.create
    ~name:"request_color"
    ~version
    ~bin_query:Color.bin_t
    ~bin_response:(Option.bin_t Int.bin_t)

(* type client_message =
 *   | Request_color of Color.t
 *   | Request_state
 *   | Move of Color.t * int * (Location.t * int) list
 * [@@deriving sexp, bin_io] *)

