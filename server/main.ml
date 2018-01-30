open Core
open Async
open Log.Global

module W = Websocket_async

module Secrets = struct
  open Backgammon
  module Color_map = Map.Make(Color)

  type t = int Color_map.t

  let generate () =
    Color_map.of_alist_exn
      [Color.White, Random.bits ();
       Color.Black, Random.bits ()]
end

module Pipe_manager = struct
  type t = {mutable counter : int;
            mutable pipes : W.Frame.t Pipe.Writer.t Int.Map.t}
  let create () = {counter = -1; pipes = Int.Map.empty}
  let add t pipe =
    t.counter <- t.counter + 1;
    t.pipes <- Int.Map.set t.pipes ~key:t.counter ~data:pipe;
    t.counter
  let remove t id =
    t.pipes <- Int.Map.remove t.pipes id
end

module Game = struct
  type t = Backgammon.Game.t

  let convert_sequence sequence =
    let sequence = List.map sequence ~f:(fun (loc, i) ->
        match loc with
        | `Point _ | `Bar _ as source -> Some (source, i)
        | `Home _ -> None)
    in
    Option.all sequence

  let handle_unusable_dice state =
    let rec aux state acc =
    let open Backgammon.Game in
    match state with
    | Won _ -> state, []
    | Live g ->
      if required_steps g = 0 then
        (aux
           (Result.ok_or_failwith @@ Backgammon.Game.perform_sequence g [])
           (Backgammon.Protocol.Unusable_dice (g.turn, g.dice) :: acc))
      else state, List.rev acc
    in
    let state, messages = aux state [] in
    state, messages

  let apply_sequence state l =
    let open Result.Let_syntax in
    let%map result =
      match convert_sequence l with
      | Some sequence -> Backgammon.Game.perform_sequence state sequence
      | None -> Error "Not a valid sequence"
    in
    handle_unusable_dice result
end

let protocol_of_string s =
  match s |> Sexp.of_string |> Backgammon.Protocol.client_message_of_sexp with
  | sexp -> Ok sexp
  | exception _ -> Error "invalid sexp"

let handle_protocol ~game =
  let open Backgammon.Protocol in
  function
  | Request_state -> [Update_state !game]
  | Move l ->
    (match !game with
    | Backgammon.Game.Won _ -> [Error_message "Game is over"]
    | Backgammon.Game.Live state ->
      (match Game.apply_sequence state l with
       | Error s -> [Error_message s]
       | Ok (state, messages) -> game := state; Update_state state :: messages))

let process_string ~game s =
  let open Result.Let_syntax in
  let open Backgammon.Protocol in
  let result =
    let%map protocol = protocol_of_string s in
    handle_protocol ~game protocol
  in
  (match result with
   | Ok state -> state
   | Error message -> [Error_message message])
  |> [%sexp_of: server_message list]
  |> Sexp.to_string

let string_of_game = Fn.compose Sexp.to_string [%sexp_of: Backgammon.Game.t]

type message =
  | Response of W.Frame.t
  | Broadcast of W.Frame.t

let handle_client ~game ~pipes addr reader writer =
  let addr_str = Socket.Address.(to_string addr) in
  info "Client connection from %s" addr_str;
  let app_to_ws, sender_write = Pipe.create () in
  let receiver_read, ws_to_app = Pipe.create () in
  let pipe_id = Pipe_manager.add pipes sender_write in
  let check_request req =
    let req_str = Format.asprintf "%a" Cohttp.Request.pp_hum req in
    info "Incoming connnection request: %s" req_str ;
    Deferred.return (Cohttp.Request.(uri req |> Uri.path) = "/ws")
  in
  let rec loop () =
    Pipe.read receiver_read >>= function
    | `Eof ->
      info "Client %s disconnected" addr_str;
      Pipe_manager.remove pipes pipe_id;
      Deferred.unit
    | `Ok ({ W.Frame.opcode; extension; final; content } as frame) ->
      let open W.Frame in
      debug "<- %s" W.Frame.(show frame);
      let frame', closed =
        match opcode with
        | Opcode.Ping -> Some (Response (create ~opcode:Opcode.Pong ~content ())), false
        | Opcode.Close ->
          (* Immediately echo and pass this last message to the user *)
          Pipe_manager.remove pipes pipe_id;
          if String.length content >= 2 then
            Some (Response (create ~opcode:Opcode.Close
                              ~content:(String.sub content 0 2) ())), true
          else
          Some (Response (close 100)), true
        | Opcode.Pong -> None, false
        | Opcode.Text
        | Opcode.Binary -> Some (Broadcast {frame with content = process_string game content}), false
        | _ -> Some (Response (close 1002)), false
      in
      print_endline @@ Sexp.to_string @@ [%sexp_of: int list] @@ Int.Map.keys pipes.pipes;
      begin
        match frame' with
        | None ->
          Deferred.unit
        | Some (Response frame') ->
          debug "-> %s" (show frame');
          Pipe.write_if_open sender_write frame'
        | Some (Broadcast frame') ->
          Deferred.List.iter ~f:(fun pipe -> Pipe.write_if_open pipe frame') @@ Int.Map.data pipes.pipes
      end >>= fun () ->
      if closed then Deferred.unit
      else loop ()
  in
  Deferred.any [
    begin W.server ~log:Lazy.(force log)
        ~check_request ~app_to_ws ~ws_to_app ~reader ~writer () >>= function
      | Error err when Error.to_exn err = Exit -> Deferred.unit
      | Error err -> Error.raise err
      | Ok () -> Deferred.unit
    end ;
    loop () ;
  ]

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
    let game =
      match game_state with
      | Some state -> ref state
      | None -> ref @@ Backgammon.Game.make_starting_state ()
    in
    let pipes = Pipe_manager.create () in
    let port = 3000 in
    Tcp.(Server.create
           ~on_handler_error:`Ignore
           Where_to_listen.(of_port port) (handle_client ~game ~pipes)) >>=
    Tcp.Server.close_finished
  in
  Command.async_spec ~summary:"Backgammon server" spec run

let () = Command.run command
