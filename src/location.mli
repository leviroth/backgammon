open Color

type t =
  | Bar of color
  | Point of point_int
  | Home of color
[@@deriving sexp, compare]
and point_int = private int

val point : int -> t

val valid_points : t list

val valid : t list

val find_dest : t -> int -> color -> t option
