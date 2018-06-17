open Base
open Backgammon
open Vdom

let log_string s =
  s
  |> Ojs.string_to_js
  |> Js_browser.Console.log Js_browser.console

let send_on_protocol socket protocol_message =
  let s =
    protocol_message
    |> [%sexp_of: Protocol.client_message]
    |> Sexp.to_string
  in
  log_string @@ Printf.sprintf "sending %s" s;
  Js_browser.WebSocket.send socket s

let send_moves socket color secret moves =
  let move =
    Protocol.Move (color, secret, (moves :> (Location.t * int) list))
  in
  send_on_protocol socket move

let sexpify = Parsexp.Single.parse_string_exn

(* Model *)

type model =
  { game_state : Game.t
  ; color : Color.t option
  ; secret : int option
  ; pending_move : (Location.source * int) list
  ; selected_source : Location.source option
  ; messages : string list
  ; socket : Js_browser.WebSocket.t
  }

let init socket =
  { game_state = Game.(Live { board = starting_board;
                              dice = initial_roll ();
                              turn = Color.White});
    color = None;
    secret = None;
    pending_move = [];
    selected_source = None;
    messages = [];
    socket;}

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
    | Send_color_secret (color, secret) -> {m with color = Some color;
                                                    secret = Some secret;}
    | Unusable_dice (color, (d1, d2)) ->
      let message =
        Printf.sprintf "%s rolled the unusable dice (%d, %d)"
          (Color.string_of_t color)
          d1 d2
      in
      {m with messages = message :: m.messages}
    | Error_message message -> log_string message; m
  in
  match a with
  | `Get_color_secret c ->
    send_on_protocol m.socket @@ Protocol.Request_color c;
    m
  | `Prepare_move (source, die) ->
    let open Game in
    (match m.color, m.secret with
      | Some c, Some s ->
        (match m.game_state with
        | Won _ -> m
        | Live g ->
          let pending_move = (source, die) :: m.pending_move in
          if List.length pending_move = required_steps g
          then send_moves m.socket c s @@ List.rev pending_move;
          {m with pending_move;
                  selected_source = None;})
      | _, _ -> m)
  | `Select_source s ->
    {m with selected_source = Some s}
  | `Cancel_source -> {m with selected_source = None}
  | `Server_message message -> apply_server_message m message


let app socket =
  simple_app
    ~init:(init socket)
    ~view:view
    ~update:update
    ()

let run () =
  let hostname = Js_browser.(Location.hostname @@ Window.location window) in
  let url = Printf.sprintf "ws://%s:3000/ws" hostname in
  let socket = Js_browser.WebSocket.create url () in
  let app_instance = Vdom_blit.run @@ app socket in
  app_instance
  |> Vdom_blit.dom
  |> Js_browser.Element.append_child (Js_browser.Document.body Js_browser.document);
  Js_browser.WebSocket.add_event_listener
    socket
    "message"
    (fun message_event ->
       let open Js_browser.Event in
       let state_string = Ojs.string_of_js @@ data message_event in
       log_string state_string;
       let result =
         state_string
         |> sexpify
         |> [%of_sexp: Protocol.server_message list]
       in
       List.iter result ~f:(fun message -> Vdom_blit.process app_instance (`Server_message message)))
    false;

  Js_browser.WebSocket.add_event_listener
    socket
    "open"
    (fun _ -> send_on_protocol socket Protocol.Request_state)
    false;

  Js_browser.WebSocket.add_event_listener
    socket
    "close"
    (fun close_event ->
       Js_browser.WebSocket.CloseEvent.code close_event
       |> Int.to_string
       |> log_string)
    false

let () =
  Js_browser.Window.set_onload Js_browser.window run
