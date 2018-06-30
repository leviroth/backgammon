open Core_kernel

module T = struct
  open Bin_prot.Std

  module Pervasives = struct
    let (+) = Caml.(+)
  end

  type t =
    [ `Bar of Color.t
    | `Point of int
    | `Home of Color.t
    ] [@@deriving variants, sexp, compare, bin_io]
end

include T

type point = [`Point of int]

type source = [
  | `Bar of Color.t
  | point
]

let sexp_of_source source = sexp_of_t (source :> t)

type dest = [
  | point
  | `Home of Color.t
]

let valid_points =
  List.map ~f:(fun x-> `Point x) (List.range ~stop:`inclusive 1 24)

let valid : t list =
  [`Bar Color.White; `Bar Color.Black]
  @ valid_points
  @ [`Home Color.White; `Home Color.Black]

let find_dest source steps c =
  let step_fn = match c with | Color.White -> (-) | Color.Black -> (+) in
  match source with
  | `Bar Color.White -> `Point (25 - steps)
  | `Bar Color.Black -> `Point steps
  | `Point start ->
    let dest = step_fn start steps in
    if dest >= 1 && dest <= 24 then `Point dest else `Home c

include Comparable.Make_binable(T)
