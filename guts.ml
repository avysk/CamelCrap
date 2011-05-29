open Types
open Plotting
open Variables
open Binops

let saved_data_stacks =
  let st = Stack.create () in
  let stacks = Stack.create () in
  Stack.push st stacks ; stacks
let saved_loop_cmds = Queue.create ()
let data_stack () = Stack.top saved_data_stacks

;;
plot_init ()
;;

let pi_2 = acos 0. *. 4.

let in_loop = ref 0

let npoint center total radius rotation i =
  let r = to_float radius in
  let step = pi_2 /. total  in
  let fi = float_of_int i in
  let rot_f = to_float rotation in
  let angle = fi *. step +. rot_f in
  let delta = Point (Geometry.from_polar r angle) in
  center ++ delta

let rec execute_code code =
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
  | Multiply -> binop ( @* )
  | Divide -> binop (@/)
  | PointPlus -> binop (++)
  | PointMinus -> binop (--)
  | PointScaleUp -> binop ( @** )
  | PointScaleDown -> binop (@//)
  | MakePoint ->
      let y = pick_float () in
      let x = pick_float() in
      push (Point (x, y))
  | Assign ->
      let name = pick_string () in
      let obj = pop () in
      write_var name obj
  | Line points -> draw_line (List.map to_point points)
  | Circle rs -> draw_circles (pick_point ()) (List.map to_float rs)
  | Distance ->
      let px, py = pick_point () in
      let qx, qy = pick_point () in
      let dx = px -. qx in
      let dy = py -. qy in
      push (Number (sqrt (dx *. dx +. dy *. dy)))
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
      push (Figure (E (f1, f2, len)))
  | MakeNgon ->
      let rotation = pick_float () in
      let radius = pick_float () in
      let sides = pick_float () in
      let centerx, centery = pick_point () in
      let step = pi_2 /. sides  in
      push (Figure (P (Array.init (int_of_float sides)
                                  (fun i ->
                                    let fi = float_of_int i in
                                    let angle = fi *. step +. rotation in
                                    let dx, dy = Geometry.from_polar radius angle in
                                    (centerx +. dx, centery +. dy)))))
  | Ngonloop _ -> failwith "Loop end without loop start"
  | Pop -> ignore (pop ())
  | PrintStack -> Debug.print_stack dst
  | PrintDictionary -> print_dictionary ()
  | Pwalk ->
      let dist = to_float (pop ()) in
      let p = to_point (pop ()) in
      let f = to_figure (pop ()) in
      push (Point (Geometry.do_pwalk f p dist))
  | LoopStart ->
      let _ = print_endline "recording loop" in
      let () = Queue.clear saved_loop_cmds in
      in_loop := 1
  (* Numbers and identifiers, just push *)
  | Num x -> push (Number x)
  | Id id -> push (String id)
and process_command code =
  if !in_loop = 0
  then
    execute_code code
  else
    match code with
    | LoopStart -> in_loop := !in_loop + 1 ; Queue.push code saved_loop_cmds
    | Ngonloop (center, total, radius, rotation) ->
        if !in_loop = 1
        then
          let _ = print_endline "got to loop" in
          let tot_f = to_float total in
          let n = int_of_float tot_f in
          let vertices = Array.init n (npoint center tot_f radius rotation) in
          Loop_support.start_loop (n, vertices) ;
          let new_data_stack = Stack.create () in
          let () = Stack.push new_data_stack saved_data_stacks in
          let loop_cmds = Queue.copy saved_loop_cmds in
          in_loop := 0 ;
          for i = 1 to n do
            Queue.iter process_command loop_cmds ;
            Loop_support.iteration_end i ;
          done ;
          Loop_support.end_loop () ;
          ignore (Stack.pop saved_data_stacks)
        else
          in_loop := !in_loop - 1 ;
          Queue.push code saved_loop_cmds
    | _ -> Queue.push code saved_loop_cmds

