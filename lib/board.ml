open Base

type t = (Location.t, Piece_stack.t, Location.comparator_witness) Map.t

let sexp_of_t = Map.sexp_of_m__t (module Location) Piece_stack.sexp_of_t
let t_of_sexp = Map.m__t_of_sexp (module Location) Piece_stack.t_of_sexp

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
