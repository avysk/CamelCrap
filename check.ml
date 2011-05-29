let var_table = Hashtbl.create 16

let loop_vertices = Stack.create ()
let current_loop_count = Stack.create ()
let saved_data_stacks =
  let st = Stack.create () in
  let stacks = Stack.create () in
  Stack.push st stacks ; stacks
let saved_loop_cmds = Queue.create ()
let data_stack () = Stack.top saved_data_stacks

let get_vertex i =
  let index = i + (Stack.top current_loop_count) in
  let n, vs = (Stack.top loop_vertices) in
  let k = if index >= n then index - n else index in
  vs.(k)

let get_from_table id =
  try
    Hashtbl.find var_table id
  with Not_found ->
    failwith ("No binding for " ^ id)

let get_object id =
  if id = "#" then `Point (0., 0.)
  else if id.[0] != 'V'
  then
    get_from_table id
  else try
      let n = int_of_string (String.sub id 1 (String.length id - 1)) in
      get_vertex n
  with _ ->
    get_from_table id

let put_object id value =
  assert false
(*
  Hashtbl.replace var_table id value
*)

let get_float id =
  match get_object id with
  | `Number x -> x
  | _ -> failwith (id ^ " is not bound to float")

let get_point id =
  match get_object id with
  | `Point p -> p
  | _ -> failwith (id ^ " is not bound to point")

(*
let get_figure id =
  match get id with
  | Figure f -> f
  | _ -> failwith (id ^ " is not bound to figure")
*)


let to_float = function
  | `Number x -> x
  | `String s -> get_float s
  | _ -> failwith "Expected float; got whatever"

let to_string = function
  | `String s -> s
  | _ -> failwith "Expected string; got whatever"

let to_point = function
  | `String s -> get_point s
  | `Point p -> p
  | _ -> failwith "Expected point; got whatever"

let to_ojbect = function
  | `String s -> get_object s
  | o -> o
(*
let to_figure = function
  | String s -> get_figure s
  | Figure f -> f
  | Number _ -> failwith "Expected figure; got float"
  | Point _ -> failwith "Expected figure; got point"
*)

let float_binop x y op =
  `Number (op (to_float x) (to_float y))

let point_binop p q op =
  let px, py = (to_point p) in
  let qx, qy = (to_point q) in
  `Point ((op px qx), (op py qy))

let point_float_binop p x op =
  let px, py = (to_point p) in
  let xx = to_float x in
  `Point ((op px xx), (op py xx))

let (@+) x y = float_binop x y (+.)
let (@-) x y = float_binop x y (-.)
let (@*) x y = float_binop x y ( *. )
let (@/) x y = float_binop x y (/.)

let (++) p q = point_binop p q (+.)
let (--) p q = point_binop p q (-.)

let (@**) p x = point_float_binop p x ( *. )
let (@//) p x = point_float_binop p x (/.)

let pi_2 = acos 0. *. 4.

let in_loop = ref 0

let from_polar radius angle =
  (cos angle *. radius, sin angle *. radius)

let npoint center total radius rotation i =
  let r = to_float radius in
  let step = pi_2 /. total  in
  let fi = float_of_int i in
  let rot_f = to_float rotation in
  let angle = fi *. step +. rot_f in
  let delta = `Point (from_polar r angle) in
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
  | `Plus -> binop (@+)
  | `Minus -> binop (@-)
  | `Multiply -> binop ( @* )
  | `Divide -> binop (@/)
  | `PointPlus -> binop (++)
  | `PointMinus -> binop (--)
  | `PointScaleUp -> binop ( @** )
  | `PointScaleDown -> binop (@//)
  | `MakePoint ->
      let y = pick_float () in
      let x = pick_float() in
      push (`Point (x, y))
  | `Assign ->
      let name = pick_string () in
      let obj = pop () in
      put_object name obj
  | `DrawLine points -> ignore (List.map to_point points)
  | `DrawCircle rs -> pick_point () ; ignore (List.map to_float rs)
  | `DrawEllipse ->
      let len = pick_float () in
      let f2 = pick_point () in
      let f1 = pick_point () in
      assert false
  | `MakeCircle ->
      let r = pick_float () in
      let center = pick_point () in
      push (`Circle (center, r))
  | `MakeEllipse ->
      let len = pick_float () in
      let f2 = pick_point () in
      let f1 = pick_point () in
      push (`Ellipse (f1, f2, len))
  | `MakeNgon ->
      let rotation = pick_float () in
      let radius = pick_float () in
      let sides = pick_float () in
      let centerx, centery = pick_point () in
      let step = pi_2 /. sides  in
      push (`Polygon (Array.init (int_of_float sides)
                                 (fun i ->
                                   let fi = float_of_int i in
                                   let angle = fi *. step +. rotation in
                                   let dx, dy = from_polar radius angle in
                                   (centerx +. dx, centery +. dy))))
  | `Ngonloop _ -> failwith "Loop closed but not opened"
  | `PrintStack -> assert false
  | `LoopStart ->
      let _ = print_endline "recording loop" in
      let () = Queue.clear saved_loop_cmds in
      in_loop := 1
  (* Numbers and identifiers, just push *)
  | `Num x -> push (`Number x)
  | `Id id -> push (`String id)
and process_command code =
  if !in_loop = 0
  then
    execute_code code
  else
    match code with
    | `LoopStart -> in_loop := !in_loop + 1 ; Queue.push code saved_loop_cmds
    | `Ngonloop (center, total, radius, rotation) ->
        if !in_loop = 1
        then
          let _ = print_endline "got to loop" in
          let tot_f = to_float total in
          let n = int_of_float tot_f in
          let vertices = Array.init n (npoint center tot_f radius rotation) in
          let () = Stack.push (n, vertices) loop_vertices in
          let new_data_stack = Stack.create () in
          let () = Stack.push new_data_stack saved_data_stacks in
          let loop_cmds = Queue.copy saved_loop_cmds in
          in_loop := 0 ;
          for i = 1 to n do
            Stack.push (i - 1) current_loop_count ;
            Queue.iter process_command loop_cmds ;
            ignore (Stack.pop current_loop_count)
          done ;
          ignore (Stack.pop loop_vertices) ;
          ignore (Stack.pop saved_data_stacks)
        else
          in_loop := !in_loop - 1 ;
          Queue.push code saved_loop_cmds
    | _ -> Queue.push code saved_loop_cmds
