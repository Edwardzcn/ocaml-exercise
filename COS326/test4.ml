type var = string
type op = Plus | Minus

type exp =
  | Int of int
  | Op of exp * op * exp
  | Var of var
  | Let of var * exp * exp

(* Extending the abstract syntax of expressions. Extend the implementation of the renaming function *)
let rec rename (x:var) (y:var) (e:exp) : exp =
  match e with
  | Op (e1, op, e2) ->
    Op (rename x y  e1, op , rename x y e2)
  | Var z ->
    if z = x then Var y else e
  | Int i ->
    Int i
  | Let (z,e1,e2) ->
    Let (z,rename x y e1, if z = x then e2 else rename x y e2)
