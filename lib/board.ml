open Core_kernel
open Bin_prot.Std

type t = Piece_stack.t Location.Map.t
[@@deriving bin_io, sexp]

let empty =
  Location.valid
  |> List.map ~f:(fun x -> (x, None))
  |> Location.Map.of_alist_exn

let get board location =
  Map.find_exn board (location :> Location.t)

let put board ~location ~contents =
  Map.set board ~key:(location :> Location.t) ~data:contents

let update board ~location ~f =
  Map.change board (location :> Location.t) ~f
