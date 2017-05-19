open Color;;
open OUnit2;;

let test_local_legality _ =
  let moved_board = Game.single_move_unsafe Game.starting_board (Location.point 24) (Location.point 23) in
  assert_equal true (Game.move_legal_local Game.starting_board (Location.point 24) 1 White);
  assert_equal false (Game.move_legal_local Game.starting_board (Location.point 24) 5 White);
  assert_equal true (Game.move_legal_local moved_board (Location.point 24) 1 White);
  assert_equal false (Game.move_legal_local Game.starting_board (Location.point 23) 5 White);
  assert_equal (Board.get moved_board (Location.point 23)) (Some (White, 1));
  assert_equal false (Game.move_legal_local Game.starting_board (Location.point 6) 6 White)
;;

let suite =
  "suite">:::
  ["test_local_legality">:: test_local_legality]
;;

let () =
  run_test_tt_main suite
;;
