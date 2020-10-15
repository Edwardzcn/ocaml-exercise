(* Logic and Codes *)

(* type*)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

(* so (a \lor b) \land ( a \land b) is written *)

let myexpr  = And( Or( Var "a" , Var "b" ) , And (Var "a", Var "b"));;


let table2 p q exp =
  let eval (valp,valq)  = function
    | i when i = p -> valp
    | j when j = q -> valq
  in
  let rec calc2 value_func  = function
    | Var c -> value_func c
    | Not expr -> not (calc2 value_func expr)
    | And (expr1,expr2) -> (calc2 value_func expr1) && (calc2 value_func expr2)
    | Or (expr1,expr2) -> (calc2 value_func expr1) || (calc2 value_func expr2)
  in

  [ (true,true, calc2 (eval (true,true)) exp);
    (true,false, calc2 (eval(true,false)) exp);
    (false,true, calc2 (eval(false,true)) exp);
    (false,false, calc2 (eval(false,false)) exp)];;

table2 "a" "b"  myexpr;;
let myexpr2 = And(Var "a",Or(Var "a",Var "b"));;
table2 "a" "b"  myexpr2;;
