type t = White | Black [@@deriving sexp, compare]

let flip_color = function | White -> Black
                          | Black -> White

let char_of_t = function | White -> 'W'
                         | Black -> 'B'
