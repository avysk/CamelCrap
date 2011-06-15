open Types

(* Point  -> Number     -> Number -> Number *)
(* center -> iterations -> radius -> rotation *)
val start_loop : thing -> thing -> thing -> thing -> unit

val iteration_end : int -> unit
val end_loop : unit -> unit

