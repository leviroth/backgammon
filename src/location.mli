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

val point : int -> [`Point of point_int]

val valid_points : [`Point of point_int] list

val valid : t list

val find_dest : source -> int -> color -> dest
