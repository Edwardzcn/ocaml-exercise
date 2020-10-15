(* Height-balanced binary trees *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let add_trees_with left right all =
    let add_right_tree all l =
      List.fold_left (fun a r -> Node('x', l, r) :: a) all right in
    List.fold_left add_right_tree all left;; (* add_right_tree as a function pass to out List.fold_left *)


let rec hbal_tree num =
  match num with
  | 0 -> [Empty]
  | 1 -> [Node('x',Empty,Empty)]
  | a ->
    let t1 = hbal_tree(num-1)
    in
    let t2 = hbal_tree(num-2)
    in
    add_trees_with t1 t1  (add_trees_with t1 t2 (add_trees_with t2 t1 []));;

let t = hbal_tree 3;;

let x = 'x';;

List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
               Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)))) t;;

List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
               Node(x, Node(x, Empty, Empty), Empty))) t;;

List.length t;;
