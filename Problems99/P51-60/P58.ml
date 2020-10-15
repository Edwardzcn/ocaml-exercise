(* Generate-and-test paradigm *)

(* construct symmetric and completely balanced binary trees *)

let rec cbal_tree num =
  let add_trees left right all =
    let add_right_tree all l =
      List.fold_left (fun a r ->  Node ('x',l,r)::a ) all right in
    List.fold_left add_right_tree all left
  in
  if num = 0 then [Empty]
  else if num mod 2 = 1 then
    let  t = cbal_tree (num/2) in
    add_trees t t []
  else
    let t1 = cbal_tree (num/2) in
    let t2 = cbal_tree (num/2-1) in
    add_trees t1 t2 (add_trees t2 t1 []);;


cbal_tree 3;;

let sym_cbal_trees num =
  List.filter is_symmetric (cbal_tree  num );;


sym_cbal_trees 5;;


List.length (sym_cbal_trees 57);;


let range low high =
  let rec aux low high acc =
    if low < high then aux (low+1) high (low::acc)
    else acc
  in
  if low > high then List.rev (aux high low [])
      else List.rev (aux low high []);;


List.map (fun n-> n, List.length(sym_cbal_trees n )) (range 10 20);;
