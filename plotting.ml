open Printf

let draw_line plist =
  printf "Drawing line:\n" ;
  List.iter (fun p -> let px, py = p in printf "point %f %f\n" px py) plist ;
  printf "Line done.\n"

let draw_circles center rs =
  let cx, cy = center in
  printf "Drawing circle with center at %f %f:\n" cx cy ;
  List.iter (printf "Radius %f\n") rs ;
  printf "Circle done.\n"

let draw_ellipse f1 f2 len =
  let f1x, f1y = f1 in
  let f2x, f2y = f2 in
  printf "Drawing ellipse, focals %f, %f and %f, %f; length %f\n" f1x f1y f2x f2y len
