open Color
open Core

let flip_color = function | White -> Black | Black -> White

let starting_board : Board.t =
  let flip_side (point, (color, count)) = (25 - point,
                                           (flip_color color, count)) in
  let expand_pair (point, stack) = (Location.point point, Some stack) in
  let black_side = [(24, (White, 2));
                    (19, (Black, 5));
                    (17, (Black, 3));
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

let using_full_value point die color =
  match color with
  | White -> point = Location.point die
  | Black -> point = Location.point (25 - die)

let no_higher_points_filled board color point =
  let higher_points = match color with
    | White -> List.range (point + 1) 6 ~stop:`inclusive
    | Black -> List.range ~stride:(-1) (25 - point - 1) 19 ~stop:`inclusive in
  higher_points
  |> List.map ~f:Location.point
  |> List.for_all ~f:(fun x -> match Board.get board x with | None -> true | Some (c, _) -> c = flip_color color)

let move_legal_individual board source die color =
  let dest = Location.find_dest source die color |> Option.value_exn in
  let source_ready = has_piece_at board source color in
  let dest_ready = dest_open board dest color in
  let bar = Location.Bar color in
  match source with
  | Location.Bar(_) -> source_ready && dest_ready
  | Location.Point(n) as point ->
    source_ready
    && dest_ready
    && Board.get board bar = None
    && (dest <> Location.Home(color) || can_bear_off board color
                                        && (using_full_value point die color
                                            || no_higher_points_filled board color (n :> int)))
  | Location.Home(_) -> false
;;

let single_move_unsafe board source dest =
  let (color, _) = Option.value_exn (Board.get board source) in
  let other = flip_color color in
  let hitting = has_piece_at board dest other in
  let piece_removed = remove_from board source in
  let piece_added = add_to piece_removed dest color in
  if hitting then add_to piece_added (Location.Bar(other)) other else piece_added

let legal_uses board color die =
  let sources = ((Location.Bar color) :: Location.valid_points) in
  List.filter sources ~f:(fun source ->
      move_legal_individual board source die color)
;;

type move_tree = Tree of Location.t * move_tree list

(** Construct a tree of possible moves, given a list of dice. Must be pruned for
    under-use of available dice. *)
let rec legal_use_tree board color dice : move_tree list =
  let next_board loc die = single_move_unsafe board loc (Option.value_exn (Location.find_dest loc die color)) in
  match dice with
  | [] -> []
  | hd::tl -> legal_uses board color hd
              |> List.map ~f:(fun move -> Tree (move, legal_use_tree (next_board move hd) color tl))

(** Find the maximum height of a list of trees; in other words, the maximum
   number of moves possible. *)
let rec tree_height = function
  | [] -> 0
  | Tree(_, rest) :: tl -> max (1 + tree_height rest) (tree_height tl)

(* True if sequence of (die to use, piece to move) is legal, given possible
   permutations in dice. *)
let move_legal_sequence board color (dice : int list list) (sequence : (int * Location.t) list) =
  let find_tree loc trees = List.find trees ~f:(fun (Tree (l, _)) -> l = loc) in
  let rec in_tree seq tree =
    match seq with
    | [] -> true
    | hd::tl -> match find_tree hd tree with
      | None -> false
      | Some Tree(_, rest) -> in_tree tl rest
  in let steps = (List.map sequence ~f:fst) in
  let all_heights = List.map dice ~f:(fun x -> x |> legal_use_tree board color |> tree_height) in
  match List.find dice ~f:(List.is_prefix ~prefix:steps ~equal:(=)) with
  | None -> false
  | Some active_dice_sequence ->
    let tree = legal_use_tree board color active_dice_sequence in
    (* Sequence must only use the dice available, of course. *)
    in_tree (List.map sequence ~f:snd) tree
    (* Sequence must use maximum possible number of dice. *)
    && List.length sequence = (List.max_elt all_heights ~cmp:compare |> Option.value_exn)
    (* Sequence must use greater of two dice where possible. *)
    && (List.length steps > 1
        || let max_elt = List.max_elt steps ~cmp:compare |> Option.value_exn in
        List.hd_exn steps = max_elt || List.length (legal_uses board color max_elt) = 0)
;;
