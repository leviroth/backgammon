open Color

type t;;

val starting_board : t;;

val get_contents : t -> Location.t -> (color * int) option
