(* remove the kth element from a list *)

let remove_at num list =
  let rec aux cnt acc = function
    | [] -> acc
    | head::tail ->
      if cnt = num then aux (cnt+1) acc tail
      else aux (cnt+1) (head::acc)  tail
  in List.rev (aux 0 [] list);;

remove_at 1 ["a";"b";"c";"d"];;
remove_at 0 ["a";"b";"c";"d"];;
remove_at 10 ["a"];;

(*Easy but not tail recursive*)

let rec remove_at_ref num = function
  | [] ->[]
  | head::tail -> if num = 0 then tail else head::remove_at_ref (num-1) tail;;

remove_at_ref 1 ["a";"b";"c";"d"];;
remove_at_ref 0 ["a";"b";"c";"d"];;
remove_at_ref 10 ["a"];;
