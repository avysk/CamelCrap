open Types

let string_of_thing = function
  | Number n -> "Number: " ^ (string_of_float n)
  | String s -> "String: " ^ s
  | Point (px, py) -> "Point: (" ^ (string_of_float px) ^ ", " ^ (string_of_float py) ^ ")"
  | Figure f ->
      begin
        match f with
        | L (l1, l2) -> "Line
        | P
        | C
        | E
