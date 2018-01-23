open Base

type t = (Location.t, Piece_stack.t, Location.comparator_witness) Map.t

let empty =
  Location.valid
  |> List.map ~f:(fun x -> (x, None))
  |> Map.of_alist_exn (module Location)

let get board location =
  Map.find_exn board (location :> Location.t)

let put board ~location ~contents =
  Map.set board ~key:(location :> Location.t) ~data:contents

let update board ~location ~f =
  Map.change board (location :> Location.t) ~f
