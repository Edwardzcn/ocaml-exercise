let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;

let rec count_internals = function
  | Empty | Node(_,Empty,Empty) -> 0
  | Node(_,a,b) -> (count_internals a) + (count_internals b) + 1;;

count_internals example_tree;;


let internals tree =
  let rec internals_aux acc = function
    | Empty | Node(_,Empty,Empty) -> acc
    | Node(x,l,r) -> internals_aux (x::(internals_aux (acc) r)) l
  in
  internals_aux [] tree;;

internals example_tree;;
