open Core_kernel

type t = [
  | `Bar of Color.t
  | `Point of int
  | `Home of Color.t ]
[@@deriving variants, sexp, compare, bin_io]

include Comparable.S_binable with type t := t

type point = [`Point of int]

type source = [
  | `Bar of Color.t
  | point
]

type dest = [
  | point
  | `Home of Color.t
]

val valid_points : [> point] list

val valid : t list

val find_dest : [< source] -> int -> Color.t -> dest

val sexp_of_source : source -> Sexp.t
