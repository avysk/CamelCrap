open Types

(* Number -> Number -> Number *)
val ( @+ ) : thing -> thing -> thing
val ( @- ) : thing -> thing -> thing
val ( @* ) : thing -> thing -> thing
val ( @/ ) : thing -> thing -> thing

(* Point -> Point -> Point *)
val ( ++ ) : thing -> thing -> thing
val ( -- ) : thing -> thing -> thing

(* Point -> Number -> Point *)
val ( @** ) : thing -> thing -> thing
val ( @// ) : thing -> thing -> thing
