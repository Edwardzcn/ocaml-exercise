(*Insert an aelement at a given position*)

(*If the position is larger or equal to the length of the list, insert the element at the end*)

let rec insert_at ele index = function
  | [] -> [ele]
  | head::tail -> if index = 0 then ele::head::tail
    else head::insert_at ele (index-1) tail;;

insert_at "alfa" 1 ["a";"b";"c";"d"];;
