open Base
open OUnit2
open Backgammon
open Color

let construct_board l =
  List.fold
    ~init:Board.empty
    ~f:(fun m (k, v) -> Board.put m ~location:(Location.point k) ~contents:(Some v)) l

let test_individual_legality _ =
  let moved_board = Game.single_move_unsafe Game.starting_board (Location.point 24) (Location.point 23) in
  List.iter ~f:(assert_bool "test_individual_legality") [
    Game.move_legal_individual Game.starting_board (Location.point 24) 1 White;
    not @@ Game.move_legal_individual Game.starting_board (Location.point 24) 5 White;
    Game.move_legal_individual moved_board (Location.point 24) 1 White;
    not @@ Game.move_legal_individual Game.starting_board (Location.point 23) 5 White;
    not @@ Game.move_legal_individual Game.starting_board (Location.point 6) 6 White
  ];
  assert_equal (Board.get moved_board (Location.point 23)) (Some (White, 1))

let test_sequence_legality _ =
  let open Game in
  let stripped_board = construct_board [(3, (Black, 2)); (1, (Black, 2)); (4, (White, 2))] in
  let stripped_board_2 = construct_board [(6, (White, 1)); (5, (White, 1))] in
  let stripped_board_3 = Board.put stripped_board_2 ~location:(Location.point 6) ~contents:(Some (Black, 1)) in
  let stripped_board_4 = (Board.put stripped_board ~location:(Location.point 1) ~contents:None) in
  List.iter ~f:(assert_bool "test_sequence_legality") [
    move_legal_sequence starting_board White [[5; 3]; [3; 5]] [(3, Location.point 24); (5, Location.point 13)];
    not @@ move_legal_sequence starting_board White [[5; 3]; [3; 5]] [(3, Location.point 24); (5, Location.point 11)];
    not @@ move_legal_sequence starting_board White [[5; 3]; [3; 5]] [(3, Location.point 24)];
    not @@ move_legal_sequence stripped_board White [[1; 2]; [2; 1]] [(1, Location.point 4)];
    move_legal_sequence stripped_board White [[1; 2]; [2; 1]] [(2, Location.point 4)];
    move_legal_sequence stripped_board_2 White [[1; 6]; [6; 1]] [(1, Location.point 6); (6, Location.point 5)];
    not @@ move_legal_sequence stripped_board_2 White [[1; 6]; [6; 1]] [(6, Location.point 5); (1, Location.point 6)];
    move_legal_sequence stripped_board_3 White [[1; 6]; [6; 1]] [(1, Location.point 5); (6, Location.point 4)];
    not @@ move_legal_sequence stripped_board_4 White [[1; 2]; [2; 1]] [(2, Location.point 4)]]

let test_sample_games _ = Parse_test.process_directory "test/sample_games"

let suite =
  "suite" >:::
  ["test_individual_legality" >:: test_individual_legality;
   "test_sequence_legality" >:: test_sequence_legality;
   "test_sample_games" >:: test_sample_games
  ]

let () =
  run_test_tt_main suite
