(* Gray code *)

(* Everytime only 1 bit change *)

let gray n =
  let rec gray_next_level k acc =
    if k >= n then acc
    else
      let generate (left,right) x =
        (("0"^x) ::left,("1"^x)::right)
      in
      let (left,right) =  List.fold_left generate ([],[]) acc
      in
      (*List.rev_append (gray_next_level (k+1) left) (gray_next_level (k+1) right) *)
      (* Wrong *)
      gray_next_level (k+1) (List.rev_append left right)
  in
  gray_next_level 1 ["0";"1"];;

gray 2;;
gray 3;;

(* Got it. Now I have a better comprehension of the construction of gray code *)
