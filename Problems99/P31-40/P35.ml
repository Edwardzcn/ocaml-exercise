(* Determine the prime factors of a given positive integer *)
let factors num =               (* all factors  *)
  let rec aux n acc  =
    match n with
    | 0 -> acc
    | i ->
      if num mod i = 0 then aux (i-1) (i::acc)
      else aux (i-1) acc
  in aux num [];;

factors 315;;

let prime_factors num =
  let rec aux d num acc =
    match num with
    | 1 -> acc
    | i ->
      if num mod d = 0 then aux d (num/d) (d::acc)
      else aux (d+1) num acc
  in List.rev(aux 2 num []);;

prime_factors 315;;
