open Color

type t;;

val empty_board : t;;

val get : t -> Location.t -> (color * int) option

val put : t -> Location.t -> (color * int) option

val update : t -> Location.t -> ((color * int) option -> (color * int ) option)
