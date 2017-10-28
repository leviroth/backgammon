open Core
open Lexer
open Lexing
open Game_history

let string_of_turn (color, {dice; plays}) =
  let int_of_point (p : Location.t) =
    let open Location in
    match p with
    | `Bar _ -> 25
    | `Home _ -> 0
    | `Point n -> (n :> int) in
  let player = match color with Color.White -> "White" | Color.Black -> "Black" in
  let player_string = Printf.sprintf "Player: %s" player in
  let dice_string = Printf.sprintf "Dice: %d %d" (fst dice) (snd dice) in
  let move_strings = List.mapi ~f:(fun n (start, finish) ->
      Printf.sprintf "Move %d: %d %d" n (int_of_point (start :> Location.t)) (int_of_point (finish :> Location.t))) plays in
  String.concat ~sep:"\n" @@ (player_string :: dice_string :: move_strings)

let parse lexbuf =
  Parser.lines Lexer.read lexbuf

let rec parse_and_print lexbuf =
  parse lexbuf |> List.iter ~f:(fun x -> x |> string_of_turn |> print_endline)

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx

(* part 2 *)
let () =
  Command.basic ~summary:"Parse and display JSON"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop 
  |> Command.run
