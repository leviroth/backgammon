open Color
open Core.Std

let move_legal board source dest =
  match Board.get board source with
  | Some (color, _) -> true
  | None -> false

let starting_board : Board.t =
  let flip_color = function | White -> Black | Black -> White in
  let flip_side (point, (color, count)) = (25 - point,
                                           (flip_color color, count)) in
  let expand_pair (point, stack) = (Location.point point, Some stack) in
  let white_side = [(1, (White, 2));
                    (6, (Black, 5));
                    (8, (Black, 2));
                    (12, (White, 5));] in
  let black_side = List.map ~f:flip_side white_side in
  let items = List.map ~f:expand_pair (white_side @ black_side) in
  List.fold ~init:Board.empty ~f:(fun m (k, v) -> Board.put m ~location:k ~contents:v) items
;;

let remove_from board ?(n=1) location =
  let update_fn = function
    | Some (color, count) -> if count = n then None else Some (color, count - n)
    | None -> None in
  Board.update board ~location ~f:(Option.map ~f:update_fn)
;;

let add_to board ?(n=1) location color =
  let update_fn = function
    | Some (old_color, count) when old_color = color -> Some(color, count + n)
    | Some (_, _) | None -> Some(color, n) in
  Board.update board ~location ~f:(Option.map ~f:update_fn)
;;

let move board source dest =
  Option.map (Board.get board source)
    ~f:(fun (color, _) -> (board
                           |> (fun x -> remove_from x source)
                           |> (fun x -> add_to x dest color)))
;;
