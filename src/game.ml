open Core_kernel

type live_game = {board : Board.t;
                  dice : int * int;
                  turn : Color.t}

type t = | Live of live_game
         | Won of Color.t

let is_live t = match t with | Live _ -> true
                             | Won _ -> false

let starting_board : Board.t =
  let flip_side (point, (color, count)) = (25 - point,
                                           (Color.flip_color color, count)) in
  let expand_pair (point, stack) = (Location.point point, Some stack) in
  let black_side = [(24, (Color.White, 2));
                    (19, (Color.Black, 5));
                    (17, (Color.Black, 3));
                    (13, (Color.White, 5));] in
  let white_side = List.map ~f:flip_side black_side in
  let items = List.map ~f:expand_pair (white_side @ black_side) in
  let insert_item board (location, contents) = Board.put board ~location ~contents in
  List.fold ~init:Board.empty ~f:insert_item items

let remove_from board ?(n=1) location =
  let update_fn = function
    | Some (color, count) -> if count = n then None else Some (color, count - n)
    | None -> None in
  Board.update board ~location ~f:(Option.map ~f:update_fn)

let add_to board ?(n=1) location color =
  let update_fn = function
    | Some (old_color, count) when old_color = color -> Some(color, count + n)
    | Some (_, _) | None -> Some(color, n) in
  Board.update board ~location ~f:(Option.map ~f:update_fn)

let move board source dest =
  Option.map (Board.get board source)
    ~f:(fun (color, _) -> (board
                           |> (fun x -> remove_from x source)
                           |> (fun x -> add_to x dest color)))

let has_piece_at board location color =
  match Board.get board location with
  | Some (c, count) -> c = color && count > 0
  | None -> false

let dest_open board (dest : Location.dest) color =
  match Board.get board dest with
  | Some (c, count) -> c = color || count <= 1
  | None -> true

let can_bear_off board color =
  let distant_points = List.map ~f:Location.point
      (match color with
       | Color.White -> List.range ~stop:`inclusive 7 24
       | Color.Black -> List.range ~stop:`inclusive 1 18)
  in
  List.for_all (Location.(`Bar color) :: (distant_points :> Location.t list))
    ~f:(fun x -> match Board.get board x with
        | Some (c, _) -> c = Color.flip_color color
        | None -> true)

let using_full_value point die color =
  match color with
  | Color.White -> point = Location.point die
  | Color.Black -> point = Location.point (25 - die)

let no_higher_points_filled board color point =
  let higher_points = match color with
    | Color.White -> List.range (point + 1) 6 ~stop:`inclusive
    | Color.Black -> List.range ~stride:(-1) (25 - point - 1) 19 ~stop:`inclusive in
  higher_points
  |> List.map ~f:Location.point
  |> List.for_all ~f:(fun x -> match Board.get board x with | None -> true | Some (c, _) -> c = Color.flip_color color)

let move_legal_individual board source die color =
  let dest = Location.find_dest source die color in
  let source_ready = has_piece_at board source color in
  let dest_ready = dest_open board dest color in
  let bar = Location.(`Bar color) in
  let open Location in
  match source with
  | `Bar _ -> source_ready && dest_ready
  | `Point n as point ->
    source_ready
    && dest_ready
    && Board.get board bar = None
    && (dest <> `Home color || can_bear_off board color
                               && (using_full_value point die color
                                   || no_higher_points_filled board color (n :> int)))

(** Perform a single move from source to dest, returning the new board. Assumes
    that move was already checked for legality. *)
let single_move_unsafe board (source : [< Location.source]) (dest : [< Location.dest]) =
  let (color, _) = Option.value_exn (Board.get board source) in
  let other = Color.flip_color color in
  let hitting = has_piece_at board dest other in
  let piece_removed = remove_from board source in
  let piece_added = add_to piece_removed dest color in
  if hitting then add_to piece_added (Location.(`Bar other)) other else piece_added

let legal_uses board color die =
  let bar : Location.source = Location.(`Bar color) in
  let points : Location.source list = (Location.valid_points :> Location.source list) in
  let sources = bar :: points in
  List.filter sources ~f:(fun source ->
      move_legal_individual board source die color)

type move_tree = Tree of Location.source * move_tree list

(** Construct a tree of possible moves, given a list of dice. Must be pruned for
    under-use of available dice. *)
let rec legal_use_tree board color dice : move_tree list =
  let next_board loc die = single_move_unsafe board loc (Location.find_dest loc die color) in
  match dice with
  | [] -> []
  | hd::tl -> legal_uses board color hd
              |> List.map ~f:(fun move -> Tree (move, legal_use_tree (next_board move hd) color tl))

(** Find the maximum height of a list of trees; in other words, the maximum
    number of moves possible. *)
let rec tree_height = function
  | [] -> 0
  | Tree(_, rest) :: tl -> max (1 + tree_height rest) (tree_height tl)

let all_heights board color dice = List.map dice ~f:(fun x -> x |> legal_use_tree board color |> tree_height)
let max_sequence_length board color dice = all_heights board color dice |> List.max_elt ~cmp:compare |> Option.value_exn

(* True if sequence of (die to use, piece to move) is legal, given possible
   permutations in dice. *)
let move_legal_sequence board color dice sequence =
  let find_tree loc trees = List.find trees ~f:(fun (Tree (l, _)) -> l = loc) in
  let rec in_tree seq tree =
    match seq with
    | [] -> true
    | hd::tl -> match find_tree hd tree with
      | None -> false
      | Some Tree(_, rest) -> in_tree tl rest
  in let steps = (List.map sequence ~f:fst) in
  match List.find dice ~f:(List.is_prefix ~prefix:steps ~equal:(=)) with
  | None -> false
  | Some active_dice_sequence ->
    let tree = legal_use_tree board color active_dice_sequence in
    (* Sequence must only use the dice available, of course. *)
    in_tree (List.map sequence ~f:snd) tree
    (* Sequence must use maximum possible number of dice. *)
    && List.length sequence = (max_sequence_length board color dice)
    (* Sequence must use greater of two dice where possible. *)
    && (List.length steps <> 1
        || let max_elt = List.max_elt steps ~cmp:compare |> Option.value_exn in
        List.hd_exn steps = max_elt || List.length (legal_uses board color max_elt) = 0)

let roll_die () = Random.int 6 + 1
let roll_dice () = (roll_die (), roll_die ())
let rec initial_roll () =
  let (a, b) = roll_dice () in
  if a = b then initial_roll ()
  else (a, b)

let random_color () = if Random.bool () then Color.White else Color.Black

let make_starting_state () = Live {board = starting_board;
                                   dice = initial_roll ();
                                   turn = random_color ()}

let get_dice_sequences (a, b) =
  if a = b then [[a; a; a; a]] else [[a; b]; [b; a]]

let required_steps game = max_sequence_length game.board game.turn @@ get_dice_sequences game.dice

let perform_sequence game (sequence : (int * Location.source) list) =
  let color = game.turn in
  let step b (die, start) = single_move_unsafe b start (Location.find_dest start die color) in
  let aux board sequence =
    List.fold sequence ~init:board ~f:step
  in
  if move_legal_sequence game.board game.turn (get_dice_sequences game.dice) sequence
  then let next_state = {board = aux game.board sequence;
                         turn = Color.flip_color game.turn;
                         dice = roll_dice ()} in
    let won = Board.get next_state.board Location.(`Home game.turn) = Some (game.turn, 15) in
    Ok (if won then Won game.turn else Live next_state)
  else Error "Illegal move"
