(* Create BST *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;


let construct list =            (* PERFECT *)
  let rec insert t num =
    match t with
    | Empty -> Node(num,Empty,Empty)
    | Node(a,l,r) as root ->
      if num = a then root 
      else if num < a then Node( a, insert l num , r)
      else Node (a, l ,insert r num)
  in
  List.fold_left insert Empty list;;


construct [3;2;5;7;1];;


(* Use this function to test the solution of the previous problem *)

is_symmetric ( construct [5;3;18;1;4;12;21]);;
is_symmetric ( construct [5;3;18;1;4;12]);;
