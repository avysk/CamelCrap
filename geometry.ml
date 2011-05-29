open Types

let from_polar radius angle =
  (cos angle *. radius, sin angle *. radius)

let do_pwalk f p dist = assert false
