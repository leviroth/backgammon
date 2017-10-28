%{
open Core
%}

%token <int * int> DICE
%token <int> TURN
%token <int * int> PLAY
%token EOF

%start <(Color.t * Game_history.turn) list> lines
%%

lines:
| l = line*; EOF { List.concat l }

line:
| TURN; m1 = move; { [Color.Black, m1] }
| TURN; m1 = move; m2 = move; { [Color.White, m1; Color.Black, m2] }

move:
| d = DICE; p = PLAY*; { Game_history.({dice = d;
                                        plays =
                                          let build_locations (x, y) : Location.source * Location.dest = (Location.point x, Location.point y) in
                                          List.map ~f:build_locations p}) }
