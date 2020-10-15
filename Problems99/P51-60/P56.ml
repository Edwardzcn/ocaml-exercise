(* Symmetric binary tree *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec is_mirror left right =
  match left with
  | Empty -> if right = Empty then true else false
  | Node (a,ll,lr) ->
    match right with
    | Empty -> false
    | Node (b,rl,rr) ->
      if a <> b then false
      else (is_mirror ll rr ) && (is_mirror lr rl);;

let rec is_mirror_ref left right =
  match left,right with
  | Empty,Empty -> true
  | Node(_,ll,lr), Node(_,rl,rr) ->
    (is_mirror_ref ll rr) && (is_mirror_ref lr rl)
  | _ -> false;;


is_mirror Empty Empty;;
is_mirror (Node ('x',Empty,Node('x', Empty,Empty) ))  (Node ('x' , Node('x',Empty,Empty),Empty));;
is_mirror_ref (Node ('x',Empty,Node('x', Empty,Empty) ))  (Node ('x' , Node('x',Empty,Empty),Empty));;



let rec is_symmetric  = function
  | Empty -> true
  | Node (_,l,r) -> is_mirror_ref l r ;;



is_symmetric ( Node('x', Node('x', Empty, Empty), Node('x',Empty,Empty)));;
is_symmetric (Node('x',Empty,Empty) );;
