open Base
open Async_js
open Backgammon
open Vdom

let log_string s =
  s
  |> Ojs.string_to_js
  |> Js_browser.Console.log Js_browser.console

(* let send_moves socket color secret moves =
 *   let move =
 *     Protocol.Move (color, secret, (moves :> (Location.t * int) list))
 *   in
 *   send_on_protocol socket move *)

let sexpify = Parsexp.Single.parse_string_exn


type 'msg Vdom.Cmd.t +=
  | Connect of Rpc.Connection.t
  | Get_color_secret of Color.t * Rpc.Connection.t
  | Send_moves of Protocol.move * Rpc.Connection.t

let handle ctx =
  let open Async_kernel.Deferred.Let_syntax in
  function
  | Connect connection ->
    let open Rpc in
    Pipe_rpc.dispatch_iter
      Protocol.request_state_rpc
      connection
      ()
      ~f:(fun (message : Protocol.broadcast_message Pipe_rpc.Pipe_message.t) ->
          begin
            match message with
            | Update message -> Vdom_blit.Cmd.send_msg ctx (`Server_message message)
            | Closed _ -> log_string "Closed connection"
          end;
          Pipe_rpc.Pipe_response.Continue)
    |> Async_kernel.Deferred.ignore
  | Get_color_secret (color, connection) ->
    begin
      match%bind
        Rpc.Rpc.dispatch_exn
          Protocol.request_color_rpc
          connection
          color
      with
      | Some secret -> return (Vdom_blit.Cmd.send_msg ctx (`Set_color_secret (color, secret)))
      | None -> return ()
    end
  | Send_moves (moves, connection) ->
    begin
      match%bind
        Rpc.Rpc.dispatch_exn
          Protocol.move_rpc
          connection
          moves
      with
      | Ok () -> return ()
      | Error message -> return (Vdom_blit.Cmd.send_msg ctx (`Error_message message))
    end
  | _ -> return ()


(* Model *)

type model =
  { game_state : Game.t
  ; color : Color.t option
  ; secret : int option
  ; pending_move : (Location.source * int) list
  ; selected_source : Location.source option
  ; messages : string list
  ; connection : Rpc.Connection.t
  }

let init connection =
  ( { game_state =
        Game.(Live { board = starting_board
                   ; dice = initial_roll ()
                   ; turn = Color.White
                   })
    ; color = None
    ; secret = None
    ; pending_move = []
    ; selected_source = None
    ; messages = []
    ; connection
    }
  , Connect connection
  )

(* View *)

let button txt msg =
  input [] ~a:[onclick (fun _ -> msg); type_button; value txt]

let select_color_button color =
  button (Color.string_of_t color) (`Get_color_secret color)

let view model =
  match model.game_state with
  | Won c ->
    div [text @@ Printf.sprintf "Game over - %s won" @@ Color.string_of_t c]
  | Live ({board; dice; turn} as game_state) ->
    let my_turn =
      Option.value_map
        model.color
        ~default:false
        ~f:([%compare.equal: Color.t] turn)
    in
    let board_repr =
      Common.represent_board
        ~can_move:my_turn
        ~selected_source:model.selected_source
        ~pending_move:model.pending_move
        game_state
    in
    let color_info =
      (match model.color with
       | Some c ->
         div [text @@ Printf.sprintf "Color: %s" @@ Color.string_of_t c]
       | None ->
         div [select_color_button Color.White;
              select_color_button Color.Black])
    in
    div @@ List.concat [
      [board_repr];
      [color_info];
      if my_turn then [div [text "It's your turn!"]] else [];
      List.map model.messages ~f:(fun message -> div [text message]);
    ]

(* Update *)

let update m a =
  let apply_server_message m =
    let open Protocol in
    function
    | Update_state state -> {m with game_state = state;
                                    selected_source = None;
                                    pending_move = []}
    | Unusable_dice (color, (d1, d2)) ->
      let message =
        Printf.sprintf "%s rolled the unusable dice (%d, %d)"
          (Color.string_of_t color)
          d1 d2
      in
      {m with messages = message :: m.messages}
  in
  match a with
  | `Get_color_secret color ->
    return
      m
      ~c:[Get_color_secret (color, m.connection)]
  | `Set_color_secret (color, secret) ->
    return
      { m with
        color = Some color
      ; secret = Some secret
      }
  | `Prepare_move (source, die) ->
    let open Game in
    begin
      match m.color, m.secret with
      | Some color, Some secret ->
        begin
        match m.game_state with
         | Won _ -> return m
         | Live g ->
           let pending_move = (source, die) :: m.pending_move in
           let new_model =
             { m with
               pending_move
             ; selected_source = None
             }
           in
           if List.length pending_move = required_steps g then
             let moves =
               { Protocol.
                 color
               ; secret
               ; sequence = (List.rev pending_move :> (Location.t * int) list)
               }
             in
             return
               new_model
               ~c:[Send_moves (moves, m.connection)]
           else
             return new_model
      end
      | _, _ -> return m
    end
  | `Select_source s ->
    return {m with selected_source = Some s}
  | `Cancel_source -> return {m with selected_source = None}
  | `Server_message message -> return (apply_server_message m message)
  | `Error_message message -> log_string message; return m


let app connection =
  app
    ~init:(init connection)
    ~view:view
    ~update:update
    ()

let run () =
  let open Async_kernel.Deferred.Let_syntax in
  let hostname = Js_browser.(Location.hostname @@ Window.location window) in
  let uri = Printf.sprintf "ws://%s:3000" hostname |> Uri.of_string in
  let%bind connection = Rpc.Connection.client_exn ~uri () in
  let app_instance = Vdom_blit.run @@ app connection in
  app_instance
  |> Vdom_blit.dom
  |> Js_browser.Element.append_child (Js_browser.Document.body Js_browser.document);
  return ()

let () =
  Js_browser.Window.set_onload Js_browser.window (fun () -> run () |> ignore)
