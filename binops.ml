open Types
open Variables

let float_binop x y op =
  Number (op (to_float x) (to_float y))

let point_binop p q op =
  let px, py = (to_point p) in
  let qx, qy = (to_point q) in
  Point ((op px qx), (op py qy))

let point_float_binop p x op =
  let px, py = (to_point p) in
  let xx = to_float x in
  Point ((op xx px), (op xx py))

let (@+) x y = float_binop x y (+.)
let (@-) x y = float_binop x y (-.)
let (@*) x y = float_binop x y ( *. )
let (@/) x y = float_binop x y (/.)

let (++) p q = point_binop p q (+.)
let (--) p q = point_binop p q (-.)

let (@**) p x = point_float_binop p x ( *. )
let (@//) p x = point_float_binop p x (/.)
