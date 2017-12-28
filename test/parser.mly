%{
open Core_kernel
open Backgammon

let location_of_int color n =
  let open Location in
  match n with
  | 0 -> `Home color
  | 25 -> `Bar color
  | x -> point (match color with
                | Color.White -> x
                | Color.Black -> 25 - x)

let dest_or_exn loc : Location.dest =
  match loc with
  | `Home _ | `Point _ as x -> x
  | `Bar _ -> invalid_arg "expected dest"

let source_or_exn loc : Location.source =
  match loc with
  | `Bar _ | `Point _ as x -> x
  | `Home _ -> invalid_arg "expected source"

let map_pair f (x, y) = (f x, f y)

let build_locations color (source, dest) =
  let x, y = map_pair (location_of_int color) (source, dest) in
  (source_or_exn x, dest_or_exn y)

let build_move (details, color) =
  let aux (dice, plays) =
    let open Game_history in
    (color,
     {dice;
      plays = List.map ~f:(build_locations color) plays})
  in Option.map ~f:aux details
%}

%token <int * int> DICE
%token <int> TURN
%token <int * int> PLAY
%token <int> DOUBLE
%token TAKE
%token EOF

%start <(Backgammon.Color.t * Game_history.turn) list> lines
%%

lines:
| l = line*; EOF { List.concat l }

line:
| TURN; m1 = move; { Option.to_list @@ build_move (m1, Color.Black) }
| TURN; m1 = move; m2 = move; {List.filter_map [m1, Color.White; m2, Color.Black] ~f:build_move }

move:
| d = DICE; p = PLAY*; { Some (d, p) }
| DOUBLE | TAKE { None }
