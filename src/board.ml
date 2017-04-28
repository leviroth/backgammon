open Core.Std
open Color

type board = (color * int) option Location.Map.t;;
type t = board;;

let empty_board : board =
  Location.valid
  |> List.map ~f:(fun x -> (x, None))
  |> Location.Map.of_alist_exn
;;

let starting_board =
  let flip_color = function | White -> Black | Black -> White in
  let flip_side (point, (color, count)) = (25 - point,
                                           (flip_color color, count)) in
  let expand_pair (point, stack) = (Location.Point point, Some stack) in
  let white_side = [(1, (White, 2));
                    (6, (Black, 5));
                    (8, (Black, 2));
                    (12, (White, 5));] in
  let black_side = List.map ~f:flip_side white_side in
  let items = List.map ~f:expand_pair (white_side @ black_side) in
  List.fold ~init:empty_board ~f:(fun m (k, v) -> Location.Map.add m ~key:k ~data:v) items
;;

let get_point board n =
  n
  |> (fun n -> Location.Point n)
  |> (fun x -> Location.Map.find board x)
  |> Option.value_exn
;;

List.map ~f:(fun n ->
    n
    |> (fun n -> get_point starting_board n)
    |> sexp_of_option (sexp_of_pair sexp_of_color sexp_of_int)
    |> Sexplib.Sexp.to_string
    |> print_endline)
  (List.range 1 25)
