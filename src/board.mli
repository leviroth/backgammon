open Color

type t;;

val empty : t;;

val get : t -> Location.t -> Piece_stack.t

val put : t -> location:Location.t -> contents:Piece_stack.t -> t

val update : t -> location:Location.t -> f:(Piece_stack.t option -> Piece_stack.t option) -> t
