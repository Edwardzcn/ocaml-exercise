(* Truth tables for logical expressions *)

let table list exp =
  let rec eval val_list = function
    | Var c -> List.assoc c val_list
    | Not c -> not (eval val_list c)
    | And(c1,c2) -> (eval val_list c1) && (eval val_list c2)
    | Or(c1,c2) -> (eval val_list c1) || (eval val_list c2)
  in
  let rec get_table val_list  = function
    | [] -> [ (List.rev val_list, eval val_list exp)]                  (* before recursive  caculate *)
    | h::t ->
       get_table ((h,true) ::val_list)  t @  get_table ((h,false) :: val_list) t
  in get_table [] list;;
  

let a = Var "a" and b = Var "b" and c = Var "c" in
  table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));;
