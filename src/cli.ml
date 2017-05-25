open Core
open Color

let color_to_char = function | White -> 'W' | Black -> 'B'

let quadrants color =
  let left_range, right_range = match color with | Black -> (List.range 13 19,
                                                             List.range 19 25)
                                                 | White -> (List.range ~stride:(-1) 12 6,
                                                             List.range ~stride:(-1) 6 0) in
  let left_points = left_range |> List.map ~f:Location.point in
  let right_points = right_range |> List.map ~f:Location.point in
  (left_points, right_points)

let piece_count stack =
  match stack with
  | None -> 0
  | Some (_, count) -> count

let mid_string count =
  if count <= 5 then "    "
  else Printf.sprintf (if count < 10 then " (%d)" else "(%d)") count

let mid_bar_string count =
  let inside = if count <= 5 then "  " else Printf.sprintf "%2d" count in
  String.concat [" |"; inside; "|"]

let mid_rows board points =
  List.map points ~f:(fun x -> Board.get board x |> piece_count |> mid_string)
  |> String.concat

let string_of_mid_row board color =
  let left_points, right_points = quadrants color in
  let bar_count = piece_count @@ Board.get board @@ Location.Bar color in
  String.concat ["|";
                 mid_rows board left_points;
                 mid_bar_string bar_count;
                 mid_rows board right_points;
                 " |\n"]

let spot_to_char board point row =
  match Board.get board point with
  | None -> '-'
  | Some (color, count) -> if count >= row then color_to_char color else '-'

let bar_string board row color =
  let empty = "  |  |  " in
  let pieces = match Board.get board (Location.Bar color) with | None -> 0 | Some(_, n) -> n in
  match row with
  | 0 | 12 -> if pieces > 5 then Printf.sprintf "  |%d|  " pieces else empty
  | _ -> if pieces > 5 - row then Printf.sprintf "  | %c|  " @@ color_to_char color else empty

let points_to_string board row points =
  List.map points ~f:(fun point -> spot_to_char board point row |> String.of_char)
  |> String.concat ~sep:"   "

let row_to_string board color row =
  let left_points, right_points = quadrants color in
  String.concat ["|  ";
                 points_to_string board row left_points;
                 bar_string board row color;
                 points_to_string board row right_points;
                 "  |\n"]

let string_of_board board =
  let top_label = "+-13--14--15--16--17--18-------19--20--21--22--23--24--+\n" in
  let bot_label = "+-12--11--10---9---8---7--------6---5---4---3---2---1--+" in
  let mid1 = string_of_mid_row board Black in
  let mid2 = string_of_mid_row board White in
  let rows = List.range 1 6 |> List.map ~f:(row_to_string board Black) in
  let rows2 = List.range ~stride:(-1) 5 0 |> List.map ~f:(row_to_string board White) in
  top_label ^ String.concat rows ^ mid1 ^ mid2 ^ String.concat rows2 ^ bot_label
