open Types
open Variables
open Binops

let start_loop center total radius rotation =
  let tot_f = to_float total in
  let n = int_of_float tot_f in
  let rad_f = to_float radius in
  let rot_f = to_float rotation in
  let ith_vertex i = center ++ Point (Geometry.ith_point tot_f rad_f rot_f i) in
  let vertices = Array.init n ith_vertex in
  Loop_variables.add_loop_vertices (n, vertices) ;
  Loop_variables.init_loop_count () ;
  Data.new_data_stack ()

let iteration_end i =
  Loop_variables.next_loop_count ()
(*
  drop_loop_count () ;
  Stack.push (i - 1) loop_count
*)

let end_loop () =
  Loop_variables.drop_loop_count () ;
  Loop_variables.drop_vertices () ;
  Data.drop_data_stack ()
