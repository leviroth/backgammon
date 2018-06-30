open Base
open Bin_prot.Std

module Pervasives = struct
  let (+) = Caml.(+)
end

type t = (Color.t * int) option
[@@deriving sexp, bin_io]
