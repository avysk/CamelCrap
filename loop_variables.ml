open Types

let loop_count = Stack.create ()

let loop_vertices : (int * thing array) Stack.t = Stack.create ()

let get_vertex i =
  let index = i + Stack.top loop_count in
  let n, vs = (Stack.top loop_vertices) in
  let k = if index >= n then index - n else index in
  vs.(k)

let get_loop_count () =
  Number (float_of_int (Stack.top loop_count))

let drop_loop_count () = ignore (Stack.pop loop_count)

let init_loop_count () = Stack.push 0 loop_count

let next_loop_count () = Stack.push (Stack.pop loop_count + 1) loop_count

let drop_vertices () = ignore (Stack.pop loop_vertices)

let add_loop_vertices vs =
  Stack.push vs loop_vertices

