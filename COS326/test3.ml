type variable = string

type op = P
    lus | Minus | Times


type exp =
  | Int of int
  | Op of exp*op*exp
  | Var of variable
  | Let of variable * exp *exp

type value = exp


let e1 = Int 3
let e2 = Int 17
let e3 =  (Op (e1, Plus, e2))

let concrete_syntax_test = let x = 30 in
let y =
  ( let z = 3 in z *4 )
in
y+y

let abstract_syntax_tree_test = Let("x", Int 30, Let ("y", Let("z", Int 3, Op (Var "z",Times,Int 4)), Op (Var "y" , Plus, Var "y")))
