open Color

type t = [
  | `Bar of color
  | `Point of point_int
  | `Home of color ]
[@@deriving sexp, compare]
and point_int = int

type source = [
  | `Bar of color
  | `Point of point_int
]

type dest = [
  | `Point of point_int
  | `Home of color
]

val point : int -> t

val valid_points : t list

val valid : t list

val find_dest : t -> int -> color -> t option
