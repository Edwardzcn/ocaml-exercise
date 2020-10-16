type 'a rle =
| One of 'a
| Many of int * 'a;;

let encode list =               (* not good *)
let rec aux cnt all_list = function
| [] -> [] (*only empty*)
| [a] ->
  if cnt = 0 then (One a::all_list)
  else (Many (cnt+1,a)::all_list)
| a::b::tail ->
  if a = b then aux (cnt+1) all_list (b::tail)
  else if cnt = 0 then aux 0 (One a::all_list) (b::tail)
  else aux 0 (Many (cnt+1,a)::all_list) (b::tail)
in List.rev (aux 0 [] list);;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

let encode_2 list =
let tupleit cnt element =
match cnt  with
| 0 -> One element
| a -> Many (a+1,element)
in 
let rec aux cnt all_list = function
| [] -> []
| [a] -> ((tupleit cnt a)::all_list)
| a::(b::_ as tail) ->
if a = b then aux (cnt+1) all_list tail
else aux 0 ((tupleit cnt a)::all_list) tail
in aux 0 [] list;;


encode_2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
