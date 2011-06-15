open Types
open Binops

let from_polar radius angle =
  (cos angle *. radius, sin angle *. radius)

let pi_2 = acos 0. *. 4.

let ith_point sides r a i =
  let step = pi_2 /. sides in
  let fi = float_of_int i in
  let angle = fi *. step +. a in
  from_polar r angle

let do_pwalk f p dist = assert false
