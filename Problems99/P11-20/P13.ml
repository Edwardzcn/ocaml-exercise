(* No create the sublists containing the duplicates *)
type 'a rle =
  | One of 'a
  | Many of int* 'a;;


let encode list =
  let count_tuple cnt ele =
    match cnt with
    | 0 -> One ele
    | a -> Many (a+1, ele)
  in
  let rec aux cnt ans = function
    | [] -> []
    | [a] -> (count_tuple cnt a )::ans
    | a::(b::_ as tail) ->
      if a = b then aux (cnt+1) ans tail
      else aux 0 ((count_tuple cnt a)::ans) tail
  in
  List.rev (aux 0 [] list);;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
