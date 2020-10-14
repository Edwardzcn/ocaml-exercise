(* Constract completely balanced binary trees. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;


let rec complete_tree num =
  let create  ele  left_child right_child =
    Node (ele,left_child,right_child)
  in
  if num  = 0 then Empty
  else create 'x' (cbal_tree (num-1)) (cbal_tree (num-1))
;;


complete_tree 1;;
