open Printf

let plot_init () =
  Graphics.open_graph " 800x800" ;
  Graphics.set_color Graphics.black ;
  Graphics.set_line_width 3 

let origin = 400
let scale = 40.

let to_screen x = int_of_float (scale *. x) + origin
let to_screen' len = int_of_float (scale *. len)

let draw_line plist =
  printf "Drawing line:\n" ;
  List.iter (fun p -> let px, py = p in printf "point %f %f\n" px py) plist ;
  printf "Line done.\n" ;
  Graphics.draw_poly_line (Array.of_list (List.map (fun (px, py) ->
                                                      (to_screen px, to_screen py))
                                                   plist))

let draw_circles center rs =
  let cx, cy = center in
  printf "Drawing circle with center at %f %f:\n" cx cy ;
  List.iter (printf "Radius %f\n") rs ;
  printf "Circle done.\n" ;
  let scx = to_screen cx in
  let scy = to_screen cy in
  let srs = List.map to_screen' rs in
  List.iter (fun sr -> Graphics.draw_circle scx scy sr) srs

let draw_ellipse f1 f2 len =
  let f1x, f1y = f1 in
  let f2x, f2y = f2 in
  printf "Drawing ellipse, focals %f, %f and %f, %f; length %f\n" f1x f1y f2x f2y len
