open Types

let var_table : (string, thing) Hashtbl.t = Hashtbl.create 16

let read_var_from_table id =
  try
    Hashtbl.find var_table id
  with Not_found ->
    failwith ("No binding for " ^ id)

let write_var id value =
  Hashtbl.replace var_table id value

let read_var id =
  if id = "#" then Point (0., 0.)
  else if id.[0] != 'V'
  then
    read_var_from_table id
  else try
      let n = int_of_string (String.sub id 1 (String.length id - 1)) in
      Loop_support.get_vertex n
  with _ ->
    read_var_from_table id

let get_float id =
  match read_var id with
  | Number x -> x
  | _ -> failwith (id ^ " is not bound to float")

let get_point id =
  match read_var id with
  | Point p -> p
  | _ -> failwith (id ^ " is not bound to point")

let get_figure id =
  match read_var id with
  | Figure f -> f
  | _ -> failwith (id ^ " is not bound to figure")

let print_dictionary () = Debug.print_dictionary var_table

let to_float = function
  | Number x -> x
  | String s -> get_float s
  | Point _ -> failwith "Expected float; got point"
  | Figure _ -> failwith "Expected float; got figure"

let to_string = function
  | String s -> s
  | Number _ -> failwith "Expected string; got float"
  | Point _ -> failwith "Expected string; got point"
  | Figure _ -> failwith "Expected string; got figure"

let to_point = function
  | String s -> get_point s
  | Point p -> p
  | Number _ -> failwith "Expected point; got float"
  | Figure _ -> failwith "Expected point; got figure"

let to_figure = function
  | String s -> get_figure s
  | Figure f -> f
  | Number _ -> failwith "Expected figure; got float"
  | Point _ -> failwith "Expected figure; got point"

