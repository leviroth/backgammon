open Color
open Core

let flip_color = function | White -> Black | Black -> White

let starting_board : Board.t =
  let flip_side (point, (color, count)) = (25 - point,
                                           (flip_color color, count)) in
  let expand_pair (point, stack) = (Location.point point, Some stack) in
  let black_side = [(24, (White, 2));
                    (19, (Black, 5));
                    (17, (Black, 2));
                    (13, (White, 5));] in
  let white_side = List.map ~f:flip_side black_side in
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

let has_piece_at board location color =
  match Board.get board location with
  | Some (c, count) -> c = color && count > 0
  | None -> false

let dest_open board dest color =
  match Board.get board dest with
  | Some (c, count) -> c = color || count <= 1
  | None -> true

let home_table color =
  match color with
  | White -> List.range ~stop:`inclusive 1 6
  | Black -> List.range ~stop:`inclusive 19 24

let move_legal_local board source dest color =
  let can_bear_off board color =
    let distant_points = List.map ~f:Location.point
        (match color with
         | White -> List.range ~stop:`inclusive 7 24
         | Black -> List.range ~stop:`inclusive 1 18)
    in
    List.for_all ((Location.Bar color) :: distant_points)
      ~f:(fun x -> match Board.get board x with
          | Some (c, _) -> c = flip_color color
          | None -> true)
  in
  let source_ready = has_piece_at board source color in
  let dest_ready = dest_open board dest color in
  let bar = Location.Bar color in
  match source with
  | Location.Bar(_) -> source_ready && dest_ready
  | Location.Point(_) -> source_ready
                         && dest_ready
                         && Board.get board bar = None
                         && (dest <> Location.Home(color) || can_bear_off board color)
  | Location.Home(_) -> false
;;

let single_move_unsafe board source dest =
  let (color, _) = Option.value_exn (Board.get board source) in
  let other = flip_color color in
  let hitting = has_piece_at board dest other in
  let piece_removed = remove_from board source in
  let piece_added = add_to piece_removed dest color in
  if hitting then add_to piece_added (Location.Bar(other)) other else piece_added
