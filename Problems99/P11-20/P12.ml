(* type 'a rle =
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


encode_2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];; **)

(* Decode  *)
type 'a rle =                   (* not elegant  *)
| One of 'a
| Many of int * 'a;;

let decode list =
let rec  decode_tuple cnt ele cnt_list =
match cnt with
| 1 ->  ele::cnt_list
| _ -> decode_tuple (cnt-1) ele (ele::cnt_list)
in
let rec link_list all_list  = function
| [] -> all_list (* only empty set  careful  this is not [] *)
| Many (num,ele)::tail -> link_list ((decode_tuple num ele [])@all_list) tail
| One ele::tail ->  link_list (ele::all_list) tail
in List.rev(link_list [] list);;

decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
decode [One "b"];;


let decode_ans list =
    let rec many acc n x =
      if n = 0 then acc else many (x :: acc) (n-1) x in
    let rec aux acc = function
      | [] -> acc
      | One x :: t -> aux (x :: acc) t
      | Many (n,x) :: t -> aux (many acc n x) t  in
    aux [] (List.rev list);;

decode_ans [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
decode_ans [One "b"];;
