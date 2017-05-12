open Color
open Core

module T : sig
  type t =
    | Bar of color
    | Point of point_int
    | Home of color
  [@@deriving sexp, compare]
  and point_int = private int
  val point : int -> t
end = struct
  type t =
    | Bar of color
    | Point of point_int
    | Home of color
  [@@deriving sexp, compare]
  and point_int = int
  let point n =
    if 1 <= n && n <= 24 then Point n else invalid_arg "Invalid point"
end

include T;;

let valid : t list =
  [Bar White; Bar Black]
  @ (List.map ~f:(fun x -> point x) (List.range ~stop:`inclusive 1 24))
  @ [Home White; Home Black]

let find_dest source steps c =
  let step_fn = match c with | White -> (+) | Black -> (-) in
  match source with
  | Bar White -> Some (point steps)
  | Bar Black -> Some (point (-steps))
  | Point (start) -> let dest = step_fn (start :> int) steps in
    let open Int in
    if dest >= 1 && dest <= 24 then Some (point dest) else Some (Home c)
  | Home _ -> None
