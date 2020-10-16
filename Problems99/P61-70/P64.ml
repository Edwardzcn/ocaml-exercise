(* Layout a binary tree 1 (Easy) *)

type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node('n', Node('k', Node('c', leaf 'a',
                             Node('h', Node('g', leaf 'e',Empty), Empty)),
                   leaf 'm'),
         Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty));;

let layout_binary_tree tree =
  let pos x px py = (x,px,py)
  in
  let rec recreate_tree root cnt level =
    match root with
    | Empty -> (cnt,Empty)
    | Node(x,Empty,Empty) -> (cnt+1,Node(pos x cnt level,Empty,Empty))
    | Node(x,l,r) ->
      let (lcnt,lchild) = recreate_tree l cnt (level+1)
      in
      let (rcnt,rchild) = recreate_tree r (lcnt+1) (level+1)
      in
      (rcnt, Node(pos x lcnt level,lchild,rchild))
  in (fun (_,x) -> x) (recreate_tree tree 1 1);;


layout_binary_tree example_layout_tree;;


let layout_binary_tree_ref t =
  let rec layout depth x_left = function
    | Empty -> (Empty, x_left) (* returns a pair: the laid out tree and the first free x location *)
    | Node(v,l,r) ->
      let (l',l_x_max) = layout (depth+1) x_left l
      in
      let (r',r_x_max) = layout (depth+1) (l_x_max+1) r
      in
      (Node ((v,l_x_max,depth),l',r' ) , r_x_max)
  in
  fst (layout 1 1 t);;

layout_binary_tree_ref   example_layout_tree;;
(layout_binary_tree example_layout_tree) = (layout_binary_tree_ref example_layout_tree);;

