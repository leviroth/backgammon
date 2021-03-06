open Base

type t = [
  | `Bar of Color.t
  | `Point of point_int
  | `Home of Color.t ]
[@@deriving sexp, compare]
and point_int = private int

include Comparable.S with type t := t

type point = [`Point of point_int]

type source = [
  | `Bar of Color.t
  | point
]

type dest = [
  | point
  | `Home of Color.t
]

val point : int -> [> point]

val valid_points : [> point] list

val valid : t list

val find_dest : [< source] -> int -> Color.t -> dest

val sexp_of_source : source -> Sexp.t
