open Types

let var_table = Hashtbl.create 16

let get id =
  try
    Hashtbl.find var_table id
  with Not_found ->
    failwith ("No binding for " ^ id)

let put id value =
  Hashtbl.replace var_table id value

let get_float id =
  match get id with
  | Number x -> x
  | _ -> failwith (id ^ " is not bound to float")

let get_point id =
  match get id with
  | Point p -> p
  | _ -> failwith (id ^ " is not bound to point")

let get_figure id =
  match get id with
  | Figure f -> f
  | _ -> failwith (id ^ " is not bound to figure")

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
