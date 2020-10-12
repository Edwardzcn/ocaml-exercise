(*create a list containing all integers within a given range*)

let range i j =
  let rec  aux a b =
    if a>b then [] else a::aux (a+1) b
  in
  if i > j then List.rev (aux j i) else aux i j;;
range 3 4;;
range 1 10000;;
range 1 (-1);;

(* tail recurv *)

let range_tail i j =
  let rec aux a b acc =
    if a>b then List.rev acc else aux (a+1) b (a::acc)
  in
  if i > j then List.rev (aux j i []) else (aux i j []);;

range_tail 3 5;;
range 1 1000000;;               (* stack overflow *)
range_tail 1 100000;;           (* not stack overflow *)
