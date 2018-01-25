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

module Game = struct
  type t = Backgammon.Game.t

  let convert_sequence sequence =
    let sequence = List.map sequence ~f:(fun (loc, i) ->
        match loc with
        | `Point _ | `Bar _ as source -> Some (source, i)
        | `Home _ -> None)
    in
    Option.all sequence

  let process state l =
    match convert_sequence l with
    | Some sequence -> Backgammon.Game.perform_sequence state sequence
    | None -> Error "Not a valid sequence"
end

let protocol_of_string s =
  match s |> Sexp.of_string |> Backgammon.Protocol.t_of_sexp with
  | sexp -> Ok sexp
  | exception _ -> Error "invalid sexp"

let handle_protocol ~game =
  let open Backgammon.Protocol in
  function
  | Request_state -> Ok !game
  | Move l ->
    Result.map
    (match !game with
     | Backgammon.Game.Won _ -> Error "game is over"
     | Backgammon.Game.Live state -> Game.process state l)
    ~f:(fun state -> game := state; !game)

let process_string ~game s =
  let open Result in
  protocol_of_string s >>=
  handle_protocol ~game
  |> [%sexp_of: (Backgammon.Game.t, string) Result.t]
  |> Sexp.to_string

let string_of_game = Fn.compose Sexp.to_string [%sexp_of: Backgammon.Game.t]

let handle_client ~game addr reader writer =
  let game = match game with
    | None -> ref @@ Backgammon.Game.make_starting_state ()
    | Some g -> ref g
  in
  let addr_str = Socket.Address.(to_string addr) in
  info "Client connection from %s" addr_str;
  let app_to_ws, sender_write = Pipe.create () in
  let receiver_read, ws_to_app = Pipe.create () in
  let check_request req =
    let req_str = Format.asprintf "%a" Cohttp.Request.pp_hum req in
    info "Incoming connnection request: %s" req_str ;
    Deferred.return (Cohttp.Request.(uri req |> Uri.path) = "/ws")
  in
  let rec loop () =
    Pipe.read receiver_read >>= function
    | `Eof ->
      info "Client %s disconnected" addr_str;
      Deferred.unit
    | `Ok ({ W.Frame.opcode; extension; final; content } as frame) ->
      let open W.Frame in
      debug "<- %s" W.Frame.(show frame);
      let frame', closed =
        match opcode with
        | Opcode.Ping -> Some (create ~opcode:Opcode.Pong ~content ()), false
        | Opcode.Close ->
          (* Immediately echo and pass this last message to the user *)
          if String.length content >= 2 then
            Some (create ~opcode:Opcode.Close
                          ~content:(String.sub content 0 2) ()), true
          else
          Some (close 100), true
        | Opcode.Pong -> None, false
        | Opcode.Text
        | Opcode.Binary -> Some {frame with content = process_string game content}, false
        | _ -> Some (close 1002), false
      in
      begin
        match frame' with
        | None ->
          Deferred.unit
        | Some frame' ->
          debug "-> %s" (show frame');
          Pipe.write sender_write frame'
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
  in
  let run board () =
    let game = Option.map board ~f:(fun filename ->
        filename
        |> Stdio.In_channel.read_all
        |> Sexp.of_string
        |> [%of_sexp: Backgammon.Game.t])
    in
    let port = 3000 in
    Tcp.(Server.create
           ~on_handler_error:`Ignore
           Where_to_listen.(of_port port) (handle_client ~game)) >>=
    Tcp.Server.close_finished
  in
  Command.async_spec ~summary:"Backgammon server" spec run

let () = Command.run command
