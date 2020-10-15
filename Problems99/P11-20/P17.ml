(* Split a list into two parts; the length of the first is given *)

let split list num  =
  let rec aux num cnt acc = function
    | [] -> (List.rev acc,[])
    | head::tail ->
      if cnt = num-1 then (List.rev(head::acc) ,tail)
      else aux num (cnt+1) (head::acc) tail
  in aux num 0 [] list;;

split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;

(* no mod *)
let split_nomod list n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l
      else aux (i-1) (h :: acc) t
  in
  aux n [] list;;
