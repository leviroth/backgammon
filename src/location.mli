open Color

type t = [
  | `Bar of color
  | `Point of point_int
  | `Home of color ]
[@@deriving sexp, compare]
and point_int = private int

type point = [`Point of point_int]

type source = [
  | `Bar of color
  | point
]

type dest = [
  | point
  | `Home of color
]

val point : int -> point

val valid_points : point list

val valid : t list

val find_dest : source -> int -> color -> dest
