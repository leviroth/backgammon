open Core.Std
open Color

type t = (color * int) option Location.Map.t;;

let empty_board : t =
  Location.valid
  |> List.map ~f:(fun x -> (x, None))
  |> Location.Map.of_alist_exn
;;

let get : t -> Location.t -> (color * int) option =
  Location.Map.find_exn

let put board ~location ~contents =
  Location.Map.add board ~key:location ~data:contents

let update board ~location ~f =
  Location.Map.change board location ~f

let get_point board n =
  n
  |> (fun n -> Location.Point n)
  |> Location.Map.find board
  |> Option.value_exn
;;

let remove_from board ?(n=1) location =
  let update_fn = function
    | Some (color, count) -> if count = n then None else Some (color, count - n)
    | None -> None in
  Location.Map.change board location ~f:(Option.map ~f:update_fn)
;;

let add_to board ?(n=1) location color =
  let update_fn = function
    | Some (old_color, count) when old_color = color -> Some(color, count + n)
    | Some (_, _) | None -> Some(color, n) in
  Location.Map.change board location ~f:(Option.map ~f:update_fn)
;;

let move board source dest =
  Option.map (Location.Map.find_exn board source)
    ~f:(fun (color, _) -> (board
                          |> (fun x -> remove_from x source)
                          |> (fun x -> add_to x dest color)))
;;

let () =
  let b = Option.value_exn (move starting_board (Location.Point 1) (Location.Point 2)) in
  List.iter ~f:(fun n ->
      n
      |> (fun n -> get_point b n)
      |> sexp_of_option (sexp_of_pair sexp_of_color sexp_of_int)
      |> Sexplib.Sexp.to_string
      |> print_endline)
    (List.range 1 25)
