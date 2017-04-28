open Core.Std

type color = White | Black [@@deriving sexp, compare];;

module T = struct
  type t =
    | Bar of color
    | Point of int
    | Home of color
  [@@deriving sexp, compare]
end

include T;;
include Comparable.Make(T);;

let valid : t list =
  [Bar White; Bar Black]
  @ (List.map ~f:(fun x -> Point x) (List.range ~stop:`inclusive 0 24))
  @ [Home White; Home Black]

let find_dest source steps c =
  let step_fn = match c with | White -> (+) | Black -> (-) in
  match source with
  | Bar White -> Some (Point steps)
  | Bar Black -> Some (Point (-steps))
  | Point (start) -> let dest = step_fn start steps in
    let open Int in
    if dest >= 1 && dest <= 24 then Some (Point dest) else None
  | Home _ -> None
