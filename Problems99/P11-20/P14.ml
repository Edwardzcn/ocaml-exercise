(* Duplicate the elements of a list *)

let rec duplicate = function
  | [] -> []
  | a::tail -> a::a::duplicate tail;;

duplicate ["a";"b";"c";"c";"d"];;
