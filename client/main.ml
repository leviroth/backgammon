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
    |> Core_kernel.Sexp.to_string
    |> (fun s -> log_string @@ Printf.sprintf "sending %s" s; Js.string s)
  in
  socket##send(s)

let send_moves socket color secret moves =
  let move =
    Protocol.Move (color, secret, (moves :> (Location.t * int) list))
  in
  send_on_protocol socket move

let sexpify = Core_kernel.Sexp.of_string

(* Model *)

type model =
  { game_state : Game.t;
    color : Color.t option;
    secret : int option;
    pending_move : (Location.source * int) list;
    selected_source : Location.source option;
    messages : string list;
    socket : Js_of_ocaml.WebSockets.webSocket Js_of_ocaml.Js.t}

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

type action =
  | Cancel_source
  | Get_color_secret of Color.t
  | Prepare_move of Location.source * int
  | Select_source of Location.source
  | Server_message of Protocol.server_message

type clickable =
  | Choose_source of Set.M(Location).t
  | Choose_dest of Location.source * int Map.M(Location).t

let extend_set ~set l =
  List.fold l ~init:set ~f:(fun set item -> Set.add set item)

let clickables half_move board color (dice : Set.M(Int).t) =
  match half_move with
  | None ->
    Choose_source
      (Set.fold
          dice
          ~init:(Set.empty (module Location))
          ~f:(fun set die ->
              extend_set
                ~set
                (Game.legal_uses board color die :> Location.t list)))

  | Some source ->
    Choose_dest
      (source,
        Set.fold
          dice
          ~init:(Map.empty (module Location))
          ~f:(fun map die ->
              if Game.move_legal_individual board source die color
              then
                Map.set
                  map
                  ~key:(Location.find_dest source die color :> Location.t)
                  ~data:die
              else map))

let sequences_after_die sequences die =
  let tail_or_nothing =
    function
    | hd :: tl when hd = die -> Some tl
    | _ :: _ | [] -> None
  in
  List.filter_map sequences ~f:tail_or_nothing

let sequences_after_dice sequences dice =
  List.fold dice ~init:sequences ~f:sequences_after_die

(* View *)

let button txt msg =
  input [] ~a:[onclick (fun _ -> msg); type_button; value txt]

let select_color_button color =
  button (Color.string_of_t color) (Get_color_secret color)

let class_multi l =
  class_ @@ String.concat ~sep:" " l

let circle color loc =
  div
    ~a:[class_multi ["circle"; Color.string_of_t ~caps:false color]]
    []

let represent_stack stack loc =
  let n_circles color loc =
    List.init ~f:(fun _ -> circle color loc)
  in
  let open Color in
  div
    ~a:[class_ "pieces"]
    (match stack with
      | None -> []
      | Some (color, n) when n <= 5 ->
        n_circles color loc n
      | Some (color, n) ->
        (text @@ Printf.sprintf "x%d" n) :: n_circles color loc 5)

let location ~board ~clickables location =
  let pieces = Board.get board location in
  let base_class =
    match location with
    | `Point _ -> "point"
    | `Home c -> "home"
    | `Bar c -> "bar"
  in
  let this_onclick, this_class =
    match clickables, location with
    | Choose_source s, (`Point _ | `Bar _ as source) when Set.mem s source ->
      [onclick @@ fun _ -> Select_source source], []
    | Choose_dest (source, dests), (`Point _ | `Home _ as dest) ->
      (match Map.find dests dest with
        | Some die ->
          [onclick @@ fun _ -> Prepare_move (source, die)], ["possible"]
        | None ->
          [], [])
    | _ -> [], []
  in
  let this_class = class_multi @@ base_class :: this_class in
  let contents =
    let extra =
      match location with
      | `Point i ->
        let bgcolor = if (i :> int) % 2 = 0 then "light" else "dark" in
        [div ~a:[class_multi ["point-bg"; bgcolor]] []]
      | `Home _ | `Bar _ -> []
    in
    extra @ [represent_stack pieces location]
  in
  div
    ~a:(this_class :: this_onclick)
    contents

let represent_board ~clickables ~pending_move Game.{board; dice; turn} =
  let intermediate_board color board (source, die) =
    let open Game in
    let dest = Location.find_dest source die color in
    single_move_unsafe board source dest
  in
  let board =
    List.fold
      (List.rev pending_move)
      ~init:board
      ~f:(intermediate_board turn)
  in
  let row side =
    let range1, range2, color =
      match side with
      | `lower ->
        List.range ~stop:`inclusive ~stride:(-1) 12 7,
        List.range ~stop:`inclusive ~stride:(-1) 6 1,
        Color.White
      | `upper ->
        List.range ~stop:`inclusive 13 18,
        List.range ~stop:`inclusive 19 24,
        Color.Black
    in
    let row_model =
      (List.map range1 ~f:Location.point)
      @ [`Bar color]
      @ (List.map range2 ~f:Location.point)
      @ [`Home color]
    in
    List.map row_model ~f:(location ~board ~clickables)
  in
  div ~a:[class_ "board"; onclick (fun _ -> Cancel_source)]
  @@ List.concat [
    List.map [`upper; `lower] ~f:(fun side ->
        let side_name =
          match side with
          | `lower -> "lower"
          | `upper -> "upper"
        in
        div
          ~a:[class_multi ["row"; side_name]]
          (row side));
    [text @@ Printf.sprintf "Dice: %d %d" (fst dice) (snd dice)];
  ]

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
    let consumed_dice = List.rev_map model.pending_move ~f:snd in
    let sequences =
      dice
      |> Game.get_dice_sequences
      |> fun sequences -> sequences_after_dice sequences consumed_dice
    in
    let dice_set =
      List.concat sequences
      |> Set.of_list (module Int)
    in
    let clickables =
      if my_turn then clickables model.selected_source board turn dice_set
      else Choose_source (Set.empty (module Backgammon.Location))
    in
    let board_repr =
      represent_board ~clickables ~pending_move:model.pending_move game_state
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
      List.map model.messages ~f:(fun message -> div [text message]);
      [color_info];
      if my_turn then [div [text "It's your turn!"]] else []
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
  | Get_color_secret c ->
    send_on_protocol m.socket @@ Protocol.Request_color c;
    m
  | Prepare_move (source, die) ->
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
  | Select_source s ->
    {m with selected_source = Some s}
  | Cancel_source -> {m with selected_source = None}
  | Server_message message -> apply_server_message m message


let app socket =
  simple_app
    ~init:(init socket)
    ~view:view
    ~update:update
    ()

let run () =
  let open Js_browser in
  let hostname = Location.hostname @@ Window.location window in
  let url_string = Printf.sprintf "ws://%s:3000/ws" hostname in
  let url = Js_of_ocaml.Js.string url_string in
  let socket = new%js Js_of_ocaml.WebSockets.webSocket url in
  let app_instance = Vdom_blit.run @@ app socket in
  app_instance
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document);
  socket##.onmessage := Dom.handler (fun message_event ->
      let state_string = Js.to_string message_event##.data in
      log_string state_string;
      let result =
        state_string
        |> sexpify
        |> [%of_sexp: Protocol.server_message list]
      in
      List.iter result ~f:(fun message ->
          Vdom_blit.process app_instance (Server_message message));
      Js.bool false);
  socket##.onopen := Dom.handler (fun _ ->
      send_on_protocol socket Protocol.Request_state; Js.bool false)

let () =
  Js_browser.Window.set_onload Js_browser.window run
