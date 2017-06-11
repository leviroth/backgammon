open Core_kernel
open Color

module Comparable_location = struct
  module T = struct
    type t = Location.t [@@deriving sexp, compare]
  end
  include Comparable.Make(T)
end

module Location_map = Comparable_location.Map;;

type t = Piece_stack.t Location_map.t;;

let empty : t =
  Location.valid
  |> List.map ~f:(fun x -> (x, None))
  |> Location_map.of_alist_exn
;;

let get : t -> Location.t -> Piece_stack.t =
  Location_map.find_exn

let put board ~location ~contents : t =
  Location_map.add board ~key:location ~data:contents

let update board ~location ~f : t =
  Location_map.change board location ~f
