open Types

let loop_count = Stack.create ()

let loop_vertices : (int * thing array) Stack.t = Stack.create ()

let get_vertex i =
  let index = i + (Stack.top loop_count) in
  let n, vs = (Stack.top loop_vertices) in
  let k = if index >= n then index - n else index in
  vs.(k)

let drop_loop_count () = ignore (Stack.pop loop_count)

let drop_vertices () = ignore (Stack.pop loop_vertices)

let start_loop vs =
  Stack.push vs loop_vertices ;
  Stack.push 0 loop_count ;
  Data.new_data_stack ()

let iteration_end i =
  drop_loop_count () ;
  Stack.push (i - 1) loop_count

let end_loop () =
  drop_loop_count () ;
  drop_vertices () ;
  Data.drop_data_stack ()
