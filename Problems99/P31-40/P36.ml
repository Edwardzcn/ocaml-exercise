(* Determine the prime factors of a given positive integer 2*)
(* Run length *)

let factors n =                 (* slow *)
  let rec one_factor d num cnt =
    if num <> 1 && num mod d = 0 then one_factor d (num/d) (cnt+1)
    else (cnt,num)
  in
  let rec aux d num acc =
    match num with
    | 1 -> acc
    | i ->
      let cnt,rest = one_factor d i 0
      in
      if cnt <> 0 then
        aux (d+1) rest ((d,cnt)::acc)
      else
        aux (d+1) rest acc
  in
  List.rev(aux 2 n []);;

factors 315;;

(* Reference *)
let factors_ref n =             (* fast *)
  let rec aux d n =
    if n = 1 then [] else
    if n mod d = 0 then
      match aux d (n/d) with
      | (h,n) :: t when h  = d -> (h,n+1)::t
      | l -> (d,1) :: l
    else
      aux (d+1) n
  in
  aux 2 n;;
factors_ref 315;;

