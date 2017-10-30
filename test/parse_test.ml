open Core_kernel
open Lexer
open Lexing
open Game_history
open Backgammon

let distance a b =
  let open Location in
  let absolute = function
    | `Home Color.Black -> 25
    | `Home Color.White -> 0
    | `Bar Color.Black -> 0
    | `Bar Color.White -> 25
    | `Point (n : point_int) -> (n :> int)
  in
  Int.abs @@ absolute a - absolute b

let legal_moves l =
  (* Remove the first instance of x in l. *)
  let drop l x =
    let rec aux l x acc =
      match l with
      | [] -> List.rev acc
      | hd :: tl -> if [%compare.equal: int] hd x then List.rev_append acc tl else aux tl x (hd :: acc)
    in aux l x []
  in
  (* Convert play representation from source * dest to die * source. *)
  let convert_plays dice plays =
    let sorted_dice = List.sort ~cmp:Int.compare dice in
    let remaining_dice = ref sorted_dice in
    let raw_plays = List.map plays ~f:(fun (source, dest) -> (distance source dest, source)) in
    List.map raw_plays ~f:(fun (i, p) ->
        if (List.mem !remaining_dice i ~equal:Int.equal)
        then (remaining_dice := drop !remaining_dice i; (i, p))
        else let n = List.hd_exn !remaining_dice in remaining_dice := List.tl_exn !remaining_dice; (n, p))
  in
  let aux board (turn, move) =
    let step b (die, start) = Game.single_move_unsafe b start (Location.find_dest start die turn) in
    let sequences = Game.get_dice_sequences move.dice in
    let converted_plays = convert_plays (List.hd_exn sequences) move.plays in
    match Game.move_legal_sequence board turn sequences @@ (converted_plays :> (int * Location.t) list) with
    | false -> Error move
    | true -> Ok (List.fold converted_plays ~init:board ~f:step)
  in List.fold_result ~init:(Game.starting_board) ~f:aux l

let parse lexbuf =
  Parser.lines Lexer.read lexbuf

let rec parse_and_test lexbuf =
  parse lexbuf |> legal_moves |> Result.is_ok

let process_file filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  OUnit2.assert_bool filename @@ parse_and_test lexbuf;
  In_channel.close inx

let process_directory dirname =
  let filenames = Array.to_list @@ Sys.readdir dirname in
  filenames
  |> List.map ~f:(Printf.sprintf "%s/%s" dirname)
  |> List.iter ~f:process_file
