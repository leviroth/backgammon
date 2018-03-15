open Base
open Backgammon
open Vdom

let log_string s =
  s
  |> Ojs.string_to_js
  |> Js_browser.Console.log Js_browser.console

let sexpify = Core_kernel.Sexp.of_string

(* Model *)

type model =
  { game_state : Game.t;
    pending_move : (Location.source * int) list;
    selected_source : Location.source option;
    messages : string list;
  }

let init =
  Random.self_init ();
  { game_state = Game.make_starting_state ();
    pending_move = [];
    selected_source = None;
    messages = [];
  }

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
    let board_repr =
      Common.represent_board
        ~can_move:true
        ~selected_source:model.selected_source
        ~pending_move:model.pending_move
        game_state
    in
    div @@ List.concat [
      [board_repr];
      [div [text
            @@ Printf.sprintf "It is %s's turn."
            @@ Color.string_of_t turn]];
      List.map model.messages ~f:(fun message -> div [text message]);
    ]

(* Update *)

let handle_unusable_dice state =
  let open Game in
  let rec aux state acc =
    match state with
    | Won _ -> state, []
    | Live g ->
      if required_steps g = 0 then
        let d1, d2 = g.dice in
        let message =
          Printf.sprintf "%s rolled the unusable dice (%d, %d)"
            (Color.string_of_t g.turn)
            d1 d2
        in
        (aux
           (Result.ok_or_failwith @@ perform_sequence g [])
           (message :: acc))
      else state, List.rev acc
  in
  let state, messages = aux state [] in
  state, messages

let update m a =
  match a with
  | `Prepare_move (source, die) ->
    let open Game in
    (match m.game_state with
     | Won _ -> m
     | Live g ->
       let pending_move = (source, die) :: m.pending_move in
       if List.length pending_move = required_steps g
       then let sequence = List.rev pending_move in
         let result = Game.perform_sequence g sequence in
         let cleared = {m with pending_move = [];
                               selected_source = None;} in
         match result with
         | Error str -> {cleared with messages = str :: m.messages;}
         | Ok state ->
           let state, new_messages = handle_unusable_dice state in
           { cleared with game_state = state;
                          messages = new_messages @ cleared.messages
           }
       else {m with pending_move;
                    selected_source = None;})
  | `Select_source s ->
    {m with selected_source = Some s}
  | `Cancel_source -> {m with selected_source = None}


let app =
  simple_app
    ~init
    ~view
    ~update
    ()

let run () =
  let open Js_browser in
  app
  |> Vdom_blit.run
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)

let () =
  Js_browser.Window.set_onload Js_browser.window run
