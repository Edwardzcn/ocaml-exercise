let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;


let rec count_leaves  = function
  | Empty -> 0
  | Node(_,Empty,Empty) -> 1
  | Node(_,l,r) -> (leaves l) + (leaves r) ;;

count_leaves ( Node(1,Empty,Empty));;

let leaves tree =
  let rec aux  = function
    | Empty -> []
    | Node(a,Empty,Empty) -> [a]
    | Node(a,l,r) -> List.append (aux l) (aux r)
  in
  aux tree;;

let leaves_ref tree =
  let rec aux  acc = function
    | Empty ->  acc 
    | Node(a,Empty,Empty) -> a::acc
    | Node(a,l,r) -> aux (aux acc r) l
  in
  aux [] tree;;

count_leaves example_tree;;
leaves example_tree;;

leaves_ref example_tree;;
