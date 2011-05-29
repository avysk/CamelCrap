open Types

let saved_data_stacks =
  let st = Stack.create () in
  let stacks = Stack.create () in
  Stack.push st stacks ; stacks

let data_stack () = Stack.top saved_data_stacks

let push_data v = Stack.push v (data_stack ())
let pop_data () = Stack.pop (data_stack ())

let print_stack () = Debug.print_stack (data_stack ())

let new_data_stack () =
  let nds = Stack.create () in
  Stack.push nds saved_data_stacks

let drop_data_stack () =
  ignore (Stack.pop saved_data_stacks)
