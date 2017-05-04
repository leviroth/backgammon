open Core.Std
open Color

type t = Piece_stack.t Location.Map.t;;

let empty : t =
  Location.valid
  |> List.map ~f:(fun x -> (x, None))
  |> Location.Map.of_alist_exn
;;

let get : t -> Location.t -> Piece_stack.t =
  Location.Map.find_exn

let put board ~location ~contents : t =
  Location.Map.add board ~key:location ~data:contents

let update board ~location ~f : t =
  Location.Map.change board location ~f

let get_point board n =
  n
  |> (fun n -> Location.Point n)
  |> Location.Map.find board
  |> Option.value_exn
;;

