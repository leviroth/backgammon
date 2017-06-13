open Core_kernel

module Comparable_location = struct
  module T = struct
    type t = Location.t [@@deriving sexp, compare]
  end
  include Comparable.Make(T)
end

module Location_map = Comparable_location.Map

type t = Piece_stack.t Location_map.t

let empty =
  Location.valid
  |> List.map ~f:(fun x -> (x, None))
  |> Location_map.of_alist_exn

let get board location =
  Location_map.find_exn board (location :> Location.t)

let put board ~location ~contents =
  Location_map.add board ~key:(location :> Location.t) ~data:contents

let update board ~location ~f =
  Location_map.change board (location :> Location.t) ~f
