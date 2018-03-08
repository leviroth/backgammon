open Base
open Vdom

let pip (x, y) =
  let coords = [| 27; 61; 95 |] in
  svg_elt "circle" [] ~a:[
    int_attr "cx" coords.(x);
    int_attr "cy" coords.(y);
    int_attr "r" 15;
    attr "fill" "#000";
  ]


let pips n =
  let pip_coords =
    match n with
    | 1 -> [1, 1]
    | 2 -> [0, 0; 2, 2]
    | 3 -> [0, 0; 1, 1; 2, 2]
    | 4 -> [0, 0; 0, 2; 2, 0; 2, 2]
    | 5 -> [0, 0; 0, 2; 2, 0; 2, 2; 1, 1]
    | 6 -> [0, 0; 0, 1; 0, 2; 2, 0; 2, 1; 2, 2]
    | _ -> assert false
  in
  List.map pip_coords ~f:pip

let die n =
  let border =
    svg_elt "rect" [] ~a:[
      int_attr "width" 112;
      int_attr "height" 112;
      int_attr "x" 5;
      int_attr "y" 5;
      attr "fill" "none";
      attr "stroke" "#000"
    ]
  in
  svg_elt "svg"
    ~a:[
      int_attr "width" 100;
      int_attr "height" 100;
      attr "viewBox" "0 0 122 122"
    ]
    (border :: pips n)
