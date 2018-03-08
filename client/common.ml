open Base
open Backgammon
open Vdom

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
      [onclick @@ fun _ -> `Select_source source], []
    | Choose_dest (source, dests), (`Point _ | `Home _ as dest) ->
      (match Map.find dests dest with
        | Some die ->
          [onclick @@ fun _ -> `Prepare_move (source, die)], ["possible"]
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

let represent_board
    ~can_move
    ~selected_source
    ~pending_move
    Game.{board; dice; turn}
  =
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
  let consumed_dice = List.rev_map pending_move ~f:snd in
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
    if can_move
    then clickables selected_source board turn dice_set
    else Choose_source (Set.empty (module Backgammon.Location))
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
  div ~a:[class_ "board"; onclick (fun _ -> `Cancel_source)]
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
