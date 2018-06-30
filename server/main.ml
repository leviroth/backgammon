open Core
open Async
open Log.Global

type message_scope =
  | Response
  | Broadcast

module Secrets : sig
  open Backgammon

  type t

  val create : unit -> t
  val distribute : t -> Color.t -> int option
  val check_secret : t -> Color.t -> int -> bool
  val get_secret : t -> Color.t -> int
end = struct
  open Backgammon

  type secret_info = {secret : int; mutable distributed : bool}

  type t = secret_info array

  let create () =
    let gen_info () =
      {secret = Random.bits (); distributed = false}
    in
    [| gen_info (); gen_info () |]

  let color_index =
    function
    | Color.White -> 0
    | Color.Black -> 1

  let get_info t color =
    t.(color_index color)

  let distribute t color =
    let info = get_info t color in
    if info.distributed then None
    else (info.distributed <- true; Some info.secret)

  let get_secret t color =
    (get_info t color).secret

  let check_secret t color secret =
    get_secret t color = secret
end

module Game = struct
  open Rpc.Pipe_rpc
  type t =
    { mutable state : Backgammon.Game.t
    ; pipes : Backgammon.Protocol.broadcast_message Direct_stream_writer.Group.t
    ; secrets : Secrets.t
    }

  let create ?state ?secrets () =
    { state =
        Option.value state ~default:(Backgammon.Game.make_starting_state ())
    ; pipes = Direct_stream_writer.Group.create ()
    ; secrets = Option.value secrets ~default:(Secrets.create ())
    }

  let convert_sequence sequence =
    let sequence = List.map sequence ~f:(fun (loc, i) ->
        match loc with
        | `Point _ | `Bar _ as source -> Some (source, i)
        | `Home _ -> None)
    in
    Option.all sequence

  let handle_unusable_dice state =
    let open Backgammon in
    let rec aux (state : Game.t) acc =
      match state with
      | Won _ -> state, []
      | Live g ->
        if Game.required_steps g = 0 then
          aux
             (Result.ok_or_failwith (Game.perform_sequence g []))
             (Protocol.Unusable_dice (g.turn, g.dice) :: acc)
        else state, List.rev acc
    in
    aux state []

  let apply_sequence state l =
    let open Result.Let_syntax in
    let%map result =
      match convert_sequence l with
      | Some sequence -> Backgammon.Game.perform_sequence state sequence
      | None -> Error "Not a valid sequence"
    in
    handle_unusable_dice result

  let implement_request_state t () writer =
    let update = Backgammon.Protocol.Update_state t.state in
    begin
      match Direct_stream_writer.write_without_pushback writer update with
      | `Ok -> Direct_stream_writer.Group.add_exn t.pipes writer
      | `Closed -> ()
    end;
    return (Ok ())

  let implement_request_color t color =
    Secrets.distribute t.secrets color

  let implement_move
      t
      { Backgammon.Protocol.
        color
      ; secret
      ; sequence}
    =
    match t.state with
    | Backgammon.Game.Won _ -> Error "Game is over!"
    | Backgammon.Game.Live state ->
      if state.turn <> color then
        Error "Not your turn"
      else if not (Secrets.check_secret t.secrets color secret) then
        Error "You aren't authorized for that color"
      else
        apply_sequence state sequence
        |> Result.map ~f:(fun (state, messages) ->
            t.state <- state;
            List.iter messages ~f:(Direct_stream_writer.Group.write_without_pushback t.pipes))

  let implementations =
    let open Rpc in
    let implementations =
      let open Backgammon.Protocol in
      [ Pipe_rpc.implement_direct request_state_rpc implement_request_state
      ; Rpc.implement' request_color_rpc implement_request_color
      ; Rpc.implement' move_rpc implement_move
      ]
    in
    Implementations.create_exn ~implementations ~on_unknown_rpc:`Continue
end

let string_of_game = Fn.compose Sexp.to_string [%sexp_of: Backgammon.Game.t]

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-board" (optional string) ~doc:"starting board file"
    +> flag "-loglevel" (optional int) ~doc:"1-3 loglevel"
  in
  let set_loglevel = function
    | 2 -> set_level `Info
    | 3 -> set_level `Debug
    | _ -> ()
  in
  let run board loglevel () =
    Option.iter loglevel ~f:set_loglevel;
    let game_state = Option.map board ~f:(fun filename ->
        filename
        |> Stdio.In_channel.read_all
        |> Sexp.of_string
        |> [%of_sexp: Backgammon.Game.t])
    in
    let state =
      match game_state with
      | Some state -> state
      | None -> Backgammon.Game.make_starting_state ()
    in
    let secrets = Secrets.create () in
    let white_secret, black_secret =
      Secrets.get_secret secrets Backgammon.Color.White,
      Secrets.get_secret secrets Backgammon.Color.Black
    in
    info "Secrets:\n White: %d\n Black: %d" white_secret black_secret;
    let game = Game.create ~state ~secrets () in
    let port = 3000 in
    let where_to_listen = Tcp.Where_to_listen.(of_port port) in
    Rpc.Connection.serve
      ~implementations:Game.implementations
      ~initial_connection_state:(fun _ _ -> game)
      ~auth:(fun _ -> info "foo"; true)
      ~where_to_listen
      ~on_handshake_error:`Raise
      ()
    >>= function _ ->
    Deferred.never ()
  in
  Command.async_spec ~summary:"Backgammon server" spec run

let () =
  Random.self_init ();
  Command.run command
