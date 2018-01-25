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
    |> [%sexp_of: Protocol.t]
    |> Core_kernel.Sexp.to_string
    |> (fun s -> log_string @@ Printf.sprintf "sending %s" s; Js.string s)
  in
  socket##send(s)

let send_moves socket moves =
  send_on_protocol socket @@ Protocol.Move (moves :> (Location.t * int) list)

let sexpify = Core_kernel.Sexp.of_string

type clickable =
  | Choose_source of Set.M(Location).t
  | Choose_dest of Location.source * int Map.M(Location).t

let source_equal = (Location.equal :> Location.source -> Location.source -> bool)

let clickables half_move trees color =
  match half_move with
  | None ->
    Choose_source (List.fold trees
                     ~init:(Set.empty (module Location))
                     ~f:(fun set Game.(Tree (start, _, _)) -> Set.add set (start :> Location.t)))
  | Some source ->
    Choose_dest (source, List.fold trees
                   ~init:(Map.empty (module Location))
                   ~f:(fun map Game.(Tree (start, die, _)) ->
                       if source_equal source start
                       then Map.set map ~key:(Location.find_dest start die color :> Location.t) ~data:die
                       else map))

type model =
  {game_state : Game.t;
   color : Color.t;
   pending_move : (Location.source * int) list;
   selected_source : Location.source option;
   socket : Js_of_ocaml.WebSockets.webSocket Js_of_ocaml.Js.t}

type action =
  | Prepare_move of Location.source * int
  | Select_source of Location.source
  | Update of Game.t

let class_multi l = class_ @@ String.concat ~sep:" " l

let circle color loc =
  div
    ~a:[class_multi ["circle"; color]]
    []

let represent_stack stack loc =
  let open Color in
  div
    ~a:[class_ "pieces"]
    (match stack with
     | None -> []
     | Some (White, n) -> List.init n ~f:(fun _ -> circle "white" loc)
     | Some (Black, n) -> List.init n ~f:(fun _ -> circle "black" loc))

let dest_mem possible_dests location =
  List.mem
    ~equal:(Location.equal :> Location.dest -> Location.dest -> bool)
    possible_dests location

let point board i (clickables : clickable) =
  let location = Location.point i in
  let pieces = Board.get board location in
  let bgcolor = if i % 2 = 0 then "light" else "dark" in
  let this_class =
    "point" :: (match clickables with
        | Choose_dest (_, s) when Map.mem s location -> ["possible"]
        | _ -> [])
  in
  let this_onclick =
    match clickables with
    | Choose_source s when Set.mem s location ->
      [onclick @@ fun _ -> Select_source location]
    | Choose_dest (source, dests) ->
      (match Map.find dests location with
       | Some die -> [onclick @@ fun _ -> Prepare_move (source, die)]
       | None -> [])
    | _ -> []
  in
  div
    ~a:(this_onclick @ [class_multi this_class; attr "id" (Printf.sprintf "point-%d" i)])
    [div ~a:[class_multi ["point-bg"; bgcolor]] []; represent_stack pieces location]

let bar board color clickables =
  let location = Location.(`Bar color) in
  let pieces = Board.get board Location.(`Bar color) in
  let this_onclick =
    match clickables with
    | Choose_source s when Set.mem s location ->
      [onclick @@ fun _ -> Select_source location]
    | _ -> []
  in
  let color_name = match color with | Color.Black -> "black" | Color.White -> "white" in
  div
    ~a:(this_onclick @ [class_ "bar"; attr "id" (Printf.sprintf "bar-%s" color_name)])
    [represent_stack pieces location]

let home board color clickables =
  let location = Location.(`Home color) in
  let pieces = Board.get board Location.(`Home color) in
  let this_class =
    class_multi
      ("home" ::
       (match clickables with
        | Choose_dest (_, s) when Map.mem s location -> ["possible"]
        | _ -> []))
  in
  let this_onclick =
    match clickables with
    | Choose_dest (source, dests) ->
      (match Map.find dests location with
       | Some die -> [onclick @@ fun _ -> Prepare_move (source, die)]
       | None -> [])
    | Choose_source _ -> []
  in
  let color_name = match color with | Color.Black -> "black" | Color.White -> "white" in
  div
    ~a:(this_onclick @ [this_class; attr "id" (Printf.sprintf "home-%s" color_name)])
    [represent_stack pieces location]

let sequences_after_die sequences die =
  let tail_or_nothing =
    function
    | hd :: tl when hd = die -> Some tl
    | _ :: _ | [] -> None
  in
  List.filter_map sequences ~f:tail_or_nothing

let sequences_after_dice sequences dice =
  List.fold dice ~init:sequences ~f:sequences_after_die

let view model =
  match model.game_state with
  | Won c -> div [text "game over"]
  | Live {board; dice; turn} ->
    let intermediate_board color board (source, die) =
      let open Game in
      let dest = Location.find_dest source die color in
      single_move_unsafe board source dest
    in
    let board = List.fold model.pending_move ~init:board ~f:(intermediate_board turn) in
    let consumed_dice = List.rev_map model.pending_move ~f:snd in
    let sequences =
      dice
      |> Game.get_dice_sequences
      |> fun sequences -> sequences_after_dice sequences consumed_dice
    in
    let trees = Game.all_trees board turn sequences in
    let clickables = clickables model.selected_source trees turn in
    let row board side source pending =
      let range, color =
        match side with
        | `lower -> List.range ~stop:`inclusive ~stride:(-1) 12 1, Color.White
        | `upper -> List.range ~stop:`inclusive 13 24, Color.Black
      in
      (List.map range ~f:(fun i -> point board i clickables)) @ [home board color clickables]
    in
    div ~a:[class_ "board"]
    @@ List.concat [
      List.map [`upper; `lower] ~f:(fun side ->
          let side_name = match side with `lower -> "lower" | `upper -> "upper" in
          div ~a:[class_multi ["row"; side_name]] @@ row board side model.selected_source model.pending_move);
      List.map Color.[White; Black] ~f:(fun color -> bar board color clickables);
      (* List.map Color.[White; Black] ~f:(fun color -> home board color clickables); *)
      [text @@ Printf.sprintf "Dice: %d %d" (fst dice) (snd dice)]]

let init socket =
  {game_state = Game.(Live {board = starting_board;
                            dice = initial_roll ();
                            turn = Color.White});
   color = Color.White;
   pending_move = [];
   selected_source = None;
   socket;}

let update m a =
  match a with
  | Prepare_move (source, die) ->
    let open Game in
    (match m.game_state with
     | Won _ -> m
     | Live g ->
       let pending_move = (source, die) :: m.pending_move in
       if List.length pending_move = required_steps g
       then send_moves m.socket @@ List.rev pending_move;
       {m with pending_move;
               selected_source = None;})
  | Select_source s ->
    {m with selected_source = Some s}
  | Update state ->
    Game.{m with game_state = state; selected_source = None; pending_move = []}


let app socket = simple_app ~init:(init socket) ~view ~update ()

let run () =
  let url = Js_of_ocaml.Js.string "ws://localhost:3000/ws" in
  let socket = new%js Js_of_ocaml.WebSockets.webSocket url in
  let app_instance = Vdom_blit.run @@ app socket in
  app_instance
  |> Vdom_blit.dom
  |> Js_browser.Element.append_child (Js_browser.Document.body Js_browser.document);
  socket##.onmessage := Dom.handler (fun message_event ->
      let state_string = Js.to_string message_event##.data in
      log_string state_string;
      let result =
        state_string
        |> sexpify
        |> [%of_sexp: (Game.t, string) Result.t]
      in
      match result with
      | Error s -> log_string s; Js.bool false
      | Ok state -> Vdom_blit.process app_instance (Update state); Js.bool false);
  socket##.onopen := Dom.handler (fun _ -> send_on_protocol socket Protocol.Request_state; Js.bool false)

let () =
  Js_browser.Window.set_onload Js_browser.window run
