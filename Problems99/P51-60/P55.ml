(* Constract completely balanced binary trees. *)

(* Notice: number *)

(* In a completely balanced binary tree the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are alomost equal, which means their difference is not greater than one *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;


let rec complete_tree num =
  let create  ele  left_child right_child =
    Node (ele,left_child,right_child)
  in
  if num  = 0 then Empty
  else create 'x' (complete_tree (num-1)) (complete_tree (num-1))
;;


complete_tree 1;;

(* construct completely balanced binary trees for a given number of nodes. *)

 let add_trees_with left right all =
    let add_right_tree all l =
      List.fold_left (fun a r -> Node('x', l, r) :: a) all right in
    List.fold_left add_right_tree all left;; (* add_right_tree as a function pass to out List.fold_left *)


add_trees_with [Empty;Empty;Node('x',Empty,Empty)] [Empty;Node('x',Empty,Empty)] [];;


let rec cbal_tree num =
  if num = 0 then [Empty]
  else if num mod 2 = 1         (* left - right *)
  then let t = cbal_tree(num/2) in
    add_trees_with t t []
  else                          (* abs (left - right) = 1 *)
    let t = cbal_tree(num/2) in
    let t_1 = cbal_tree(num/2-1) in (* Attention this is num/2-1 as one node as the root *)
    add_trees_with t t_1 (add_trees_with t_1 t []);;


cbal_tree 5;;
cbal_tree 3;;
List.length (cbal_tree 40);;
