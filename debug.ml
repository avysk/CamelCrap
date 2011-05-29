open Types

let string_of_pair (x, y) =
  "(" ^ (string_of_float x) ^ ", " ^ (string_of_float y) ^ ")"

let string_of_thing = function
  | Number n -> "Number: " ^ (string_of_float n)
  | String s -> "String: " ^ s
  | Point p -> "Point: " ^ (string_of_pair p)
  | Figure f ->
      begin
        match f with
        | L (l1, l2) -> "Line: from " ^
                        (string_of_pair l1) ^
                        " to " ^
                        (string_of_pair l2)
        | P plist -> List.fold_left (fun s p -> s ^ " " ^ (string_of_pair p))
                                    "Polygon:" plist
        | C (c, r) -> "Circle: center " ^
                      (string_of_pair c) ^
                      " radius " ^
                      (string_of_float r)
        | E (f1, f2, len) -> "Ellipse: focal1 " ^
                             (string_of_pair f1) ^
                             " focal2 " ^
                             (string_of_pair f2) ^
                             " length " ^
                             (string_of_float len)
      end

let print_thing th =
  print_endline (string_of_thing th)

let print_stack st =
  print_endline "STACK: " ;
  Stack.iter print_thing st
