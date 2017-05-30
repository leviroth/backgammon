open Color
open Core

type t =
  | Bar of color
  | Point of point_int
  | Home of color
[@@deriving sexp, compare]
and point_int = int

let point n =
  if 1 <= n && n <= 24 then Point n else invalid_arg "Invalid point"

let valid_points =
  (List.map ~f:(fun x-> Point x) (List.range ~stop:`inclusive 1 24))

let valid : t list =
  [Bar White; Bar Black]
  @ valid_points
  @ [Home White; Home Black]

let find_dest source steps c =
  let step_fn = match c with | White -> (-) | Black -> (+) in
  match source with
  | Bar White -> Some (Point (25 - steps))
  | Bar Black -> Some (Point steps)
  | Point (start) -> let dest = step_fn (start :> int) steps in
    let open Int in
    if dest >= 1 && dest <= 24 then Some (Point dest) else Some (Home c)
  | Home _ -> None
