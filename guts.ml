open Types
open Variables
open Binops
open Plotting

let in_loop = ref 0

let stack_of_stacks () =
  let st = Stack.create () in
  let stacks = Stack.create () in
  Stack.push st stacks ; stacks

let saved_loop_stacks = stack_of_stacks ()
let saved_data_stacks = stack_of_stacks ()
let loop_stack () = Stack.top saved_loop_stacks
let data_stack () = Stack.top saved_data_stacks

let deref = function
  | Num x -> x
  | Id s -> get_float s
  | _ -> failwith "E1: cannot get float"

let execute_code code =
  let dst = data_stack () in
  let push v = Stack.push v dst in
  let pop () = Stack.pop dst in
  let binop f =
    let a2 = pop () in
    let a1 = pop () in
    push (f a1 a2) in
  let pick_float () = to_float (pop ()) in
  let pick_point () = to_point (pop ()) in
  let pick_string () = to_string (pop ()) in
  match code with
  | Plus -> binop (@+)
  | Minus -> binop (@-)
  | Multiply -> binop (@*)
    | Divide -> binop (@/)
    | PointPlus -> binop (++)
    | PointMinus -> binop (--)
    | PointScaleUp -> binop (@**)
    | PointScaleDown -> binop (@//)
    | MakePoint ->
        let y = pick_float () in
        let x = pick_float() in
        push (Point (x, y))
    | Assign ->
        let name = pick_string () in
        let obj = pop () in
        put name obj
    | Line points -> draw_line (List.map to_point points)
    | Circle rs -> draw_circles (pick_point ()) (List.map to_float rs)
    | Ellipse ->
        let len = pick_float () in
        let f2 = pick_point () in
        let f1 = pick_point () in
        draw_ellipse f1 f2 len
    | MakeCircle ->
        let r = pick_float () in
        let center = pick_point () in
        push (Figure (C (center, r)))
    | MakeEllipse ->
        let len = pick_float () in
        let f2 = pick_point () in
        let f1 = pick_point () in
        push (Figure (E (f2, f1, len)))
    | MakeNgon ->
        let angle = pick_float () in
        let r = pick_float () in
        let sides = pick_float () in
        let center = pick_point () in
        assert false


    (* Numbers and identifiers, just push *)
    | Num x -> push (Number x)
    | Id id -> push (String id)
;;
let process_command code =
  if !in_loop > 0
  then
    Stack.push code (loop_stack ())
  else
    execute_code code

