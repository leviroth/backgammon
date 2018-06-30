type t = White | Black [@@deriving sexp, compare, bin_io]

let flip_color = function | White -> Black
                          | Black -> White

let char_of_t = function | White -> 'W'
                         | Black -> 'B'

let string_of_t ?(caps=true) color =
  match caps, color with
  | true, White -> "White"
  | true, Black -> "Black"
  | false, White -> "white"
  | false, Black -> "black"
