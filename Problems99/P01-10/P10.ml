let encode list =
   let rec aux cur_count all_count = function
      | [] -> []
      | [a] -> (List.length(a :: cur_count) , a) :: all_count
      | a :: b :: tail ->       (* 这里可以用(b::_) as tail 然后下一行就不需要 b::tail 直接 tail *)
        if a = b then aux (a :: cur_count) all_count (b::tail)
         else aux []  ((List.length(a :: cur_count) , a) :: all_count)  (b::tail)  in
    List.rev (aux [] [] list);; (* 注意递归加入外层 所以要rev *)

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;

let encode_2 list =
  let rec aux cnt all = function
    | [] -> []
    | [a] -> (cnt+1,a)::all
    | a::b::tail ->
      if a = b then aux (cnt+1) all  (b::tail)
      else aux  0 ((cnt+1,a)::all) (b::tail)
  in List.rev ( aux 0 [] list);;
encode_2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
