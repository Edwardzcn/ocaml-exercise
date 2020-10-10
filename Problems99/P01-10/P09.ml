let pack list =
   let rec aux cur_count all_count = function
      | [] -> []
      | [a] -> (a :: cur_count) :: all_count
      | a :: b :: tail ->       (* 这里可以用(b::_) as tail 然后下一行就不需要 b::tail 直接 tail *)
        if a = b then aux (a :: cur_count) all_count (b::tail)
         else aux [] ((a :: cur_count) :: all_count) (b::tail)  in
    List.rev (aux [] [] list);; (* 注意递归加入外层 所以要rev *)

pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
