open Color;;
open OUnit2;;

let test_game _ =
  let moved_board = Game.single_move_unsafe Game.starting_board (Location.point 24) (Location.point 23) in
  assert_equal true (Game.move_legal_local Game.starting_board (Location.point 24) (Location.point 23) White);
  assert_equal false (Game.move_legal_local Game.starting_board (Location.point 24) (Location.point 19) White);
  assert_equal true (Game.move_legal_local moved_board (Location.point 24) (Location.point 23) White);
  assert_equal false (Game.move_legal_local Game.starting_board (Location.point 23) (Location.point 19) White);
  assert_equal (Board.get moved_board (Location.point 23)) (Some (White, 1));
  assert_equal false (Game.move_legal_local Game.starting_board (Location.point 6) (Location.Home White) White)
;;

let suite =
  "suite">:::
  ["test_game">:: test_game]
;;

let () =
  run_test_tt_main suite
;;
