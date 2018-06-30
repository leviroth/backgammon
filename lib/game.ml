open Base
open Bin_prot.Std

module Pervasives = struct
  let (+) = Caml.(+)
end

type live_game =
  {board : Board.t;
   dice : int * int;
   turn : Color.t}
[@@deriving sexp, bin_io]

type t =
  | Live of live_game
  | Won of Color.t
[@@deriving sexp, bin_io]

let is_live =
  function
  | Live _ -> true
  | Won _ -> false

let starting_board : Board.t =
  let flip_side (point, (color, count)) =
    (25 - point,
     (Color.flip_color color, count)) in
  let expand_pair (point, stack) = (Location.point point, Some stack) in
  let black_side = [(24, (Color.White, 2));
                    (19, (Color.Black, 5));
                    (17, (Color.Black, 3));
                    (13, (Color.White, 5));] in
  let white_side = List.map ~f:flip_side black_side in
  let items = List.map ~f:expand_pair (white_side @ black_side) in
  let insert_item board (location, contents) =
    Board.put board ~location ~contents in
  List.fold ~init:Board.empty ~f:insert_item items

let remove_from board ?(n=1) location =
  let update_fn = function
    | Some (color, count) when count <> n -> Some (color, count - n)
    | Some (_, _) | None -> None
  in
  Board.update board ~location ~f:(Option.map ~f:update_fn)

let add_to board ?(n=1) location color =
  let update_fn = function
    | Some (old_color, count) when [%compare.equal: Color.t] old_color color ->
      Some(color, count + n)
    | Some (_, _) | None -> Some(color, n)
  in
  Board.update board ~location ~f:(Option.map ~f:update_fn)

let move board source dest =
  Option.map (Board.get board source)
    ~f:(fun (color, _) -> (board
                           |> (fun x -> remove_from x source)
                           |> (fun x -> add_to x dest color)))

let has_piece_at board location color =
  match Board.get board location with
  | Some (c, count) -> [%compare.equal: Color.t] c color && count > 0
  | None -> false

let dest_open board dest color =
  match Board.get board dest with
  | Some (c, count) -> [%compare.equal: Color.t] c color || count <= 1
  | None -> true

let can_bear_off board color =
  let distant_points = List.map ~f:Location.point
      (match color with
       | Color.White -> List.range ~stop:`inclusive 7 24
       | Color.Black -> List.range ~stop:`inclusive 1 18)
  in
  Location.bar color :: distant_points
  |> List.for_all ~f:(fun x ->
      match Board.get board x with
      | Some (c, _) -> [%compare.equal: Color.t] c @@ Color.flip_color color
      | None -> true)

let using_full_value point die color =
  let full_value_point =
    match color with
    | Color.White -> die
    | Color.Black -> 25 - die
  in
  Location.equal point @@ Location.point full_value_point

let no_higher_points_filled board color point =
  let higher_points = match color with
    | Color.White -> List.range (point + 1) 6 ~stop:`inclusive
    | Color.Black -> List.range ~stride:(-1) (point - 1) 19 ~stop:`inclusive
  in
  higher_points
  |> List.map ~f:Location.point
  |> List.for_all ~f:(fun x ->
      match Board.get board x with
      | None -> true
      | Some (c, _) -> [%compare.equal: Color.t] c @@ Color.flip_color color)

let move_legal_individual board source die color =
  let dest = Location.find_dest source die color in
  let source_ready = has_piece_at board source color in
  let dest_ready = dest_open board dest color in
  let bar = Location.bar color in
  match source with
  | `Bar _ -> source_ready && dest_ready
  | `Point n as point ->
    source_ready
    && dest_ready
    && Option.is_none @@ Board.get board bar
    && (Location.((dest :> Location.t) <> home color)
        || can_bear_off board color
           && (using_full_value point die color
               || no_higher_points_filled board color n))

(** Perform a single move from source to dest, returning the new board. Assumes
    that move was already checked for legality. *)
let single_move_unsafe board source dest =
  let (color, _) = Option.value_exn (Board.get board source) in
  let other = Color.flip_color color in
  let hitting = has_piece_at board dest other in
  let piece_removed = remove_from board source in
  let piece_added = add_to piece_removed dest color in
  if hitting
  then add_to piece_added (Location.bar other) other
  else piece_added

let legal_uses board color die =
  let bar = Location.(`Bar color) in
  let points = Location.valid_points in
  let sources = bar :: points in
  List.filter sources ~f:(fun source ->
      move_legal_individual board source die color)

let max_sequence_length board color dice_orders =
  let next_board board loc die =
    single_move_unsafe board loc (Location.find_dest loc die color)
  in
  let max_depth = List.length @@ List.hd_exn dice_orders in
  let rec aux board color dice depth =
    if depth = max_depth then Error depth
    else match dice with
      | [] -> Ok depth
      | hd :: tl ->
        (match legal_uses board color hd with
         | [] -> Ok depth
         | uses ->
           List.fold_result
             uses
             ~init:0
             ~f:(fun max_so_far move ->
                 aux (next_board board move hd) color tl (depth + 1)
                 |> Result.map ~f:(max max_so_far)))
  in
  List.fold_result dice_orders ~init:0 ~f:(fun max_so_far die ->
      aux board color die 0
      |> Result.map ~f:(max max_so_far))
  |> function Ok n -> n | Error n -> n

let locally_validate_sequence board color dice_orders sequence =
  let next_board board loc die =
    single_move_unsafe board loc (Location.find_dest loc die color)
  in
  let steps = List.map sequence ~f:snd in
  match List.find dice_orders ~f:(List.is_prefix ~prefix:steps ~equal:(=)) with
  | None -> false
  | Some active_dice_sequence ->
    List.fold_result sequence ~init:board ~f:(fun board (source, die) ->
        if move_legal_individual board source die color
        then Ok (next_board board source die)
            else Error ())
    |> Result.is_ok

(* True if sequence of (die to use, piece to move) is legal, given possible
   permutations in dice. *)
let move_legal_sequence board color dice sequence =
  let steps = List.map sequence ~f:snd in
  locally_validate_sequence board color dice sequence
  (* Sequence must use maximum possible number of dice. *)
  && List.length sequence = (max_sequence_length board color dice)
  (* Sequence must use greater of two dice where possible. *)
  && (match steps with
      | [die] ->
        let greatest_in_dice =
          List.filter_map dice ~f:(List.max_elt ~compare)
          |> List.max_elt ~compare
          |> Option.value_exn
        in
        die = greatest_in_dice
        || List.length (legal_uses board color greatest_in_dice) = 0
      | _ -> true)

let roll_die () =
  Random.int 6 + 1

let roll_dice () =
  (roll_die (), roll_die ())

let rec initial_roll () =
  let (a, b) = roll_dice () in
  if a = b then initial_roll ()
  else (a, b)

let random_color () =
  if Random.bool ()
  then Color.White
  else Color.Black

let make_starting_state () =
  Live {board = starting_board;
        dice = initial_roll ();
        turn = random_color ()}

let get_dice_sequences (a, b) =
  if a = b then [[a; a; a; a]] else [[a; b]; [b; a]]

let required_steps game =
  max_sequence_length game.board game.turn @@ get_dice_sequences game.dice

let perform_sequence game sequence =
  let color = game.turn in
  let step b (start, die) =
    single_move_unsafe b start (Location.find_dest start die color)
  in
  let aux board sequence =
    List.fold sequence ~init:board ~f:step
  in
  if move_legal_sequence
      game.board
      game.turn
      (get_dice_sequences game.dice)
      sequence
  then let next_state =
         {board = aux game.board sequence;
          turn = Color.flip_color game.turn;
          dice = roll_dice ()}
    in
    let won =
      [%compare.equal: (Color.t * int) option]
        (Board.get next_state.board (Location.home game.turn))
        (Some (game.turn, 15)) in
    Ok (if won then Won game.turn else Live next_state)
  else Error "Illegal move"
