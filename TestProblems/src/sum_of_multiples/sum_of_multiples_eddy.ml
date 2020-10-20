let sum_of_multiples (xs:int list) (n:int) : int =
  if List.length xs = 0 then failwith "no multiples"
  else if n < 0 then failwith "n < 0"
  else
    let rec is_multiple num = function
      | [] -> false
      | hd::tl -> if num mod hd = 0 then true else is_multiple num tl
    in
    let rec aux num acc high =
      if num >= high then acc
      else if is_multiple num xs then aux (num+1) (acc+num) high
      else aux (num+1) acc high
    in
    (* Another way *)
    (* let rec check xs =
     *   match xs with
     *   | [] -> 0
     *   | x::_ when i mod x = 0 -> i
     *   | _::xs -> check xs in
     * in *)
    aux 1 0 n;;

sum_of_multiples [3; 5] 1000;;
sum_of_multiples [3; 5] 10;;
