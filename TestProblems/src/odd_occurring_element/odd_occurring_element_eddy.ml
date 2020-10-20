let odd_occurring_element (xs:'a list) : 'a =
  let compare a b =
    if a=b then 0 else if a < b then -1 else 1 in 
  let sort_list = List.sort compare xs in
  match sort_list with
  | [] -> failwith "No odd-occurring element"
  | [a] -> a
  | hd::tl ->
    let rec aux ele cnt = function
      | [] -> if cnt mod 2 = 1 then ele else failwith "No odd-occurring element"
      | a::b -> if ele = a then aux ele (cnt+1) b
        else if cnt mod 2 <> 1 then aux a 1 b
        else ele
    in aux hd 1 tl;;

odd_occurring_element [42; 42; 42];;
odd_occurring_element [1; 1; 2; 42; 2; 2; 2; 42; 42];;
