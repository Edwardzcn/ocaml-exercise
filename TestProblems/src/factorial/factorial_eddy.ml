let factorial (num:int) : int =
  (* Fatorial of any number n *)
  (* No big int *)
  let rec aux low high acc =
    if low > high then acc
    else
      aux (low+1) high (low*acc)
  in
  aux 1 num 1;;


factorial 0;;
factorial 10;;
