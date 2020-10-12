(* Determine whether a given integer is prime *)
let is_prime num =              (* not elegant *)
  match num with
  | 0 | 1 -> false
  | a ->
    let rec aux cnt num =
      if cnt >= num-1 then true
      else if num mod cnt = 0 then false
      else aux (cnt+1) num
    in
    aux 2 num;;
not(is_prime 1);;
is_prime 1;;
is_prime 98;;
is_prime 97;;
is_prime 23;;


let is_prime_ref num =
  let num = abs num in
  let rec is_not_divisor_ref cnt =
    cnt * cnt > num || (num mod cnt <> 0 && is_not_divisor_ref (cnt+1))
  in
  num <> 0 && num <> 1 && is_not_divisor_ref 2;;

is_prime 100000007;;            (* slow *)

is_prime_ref 100000007;;        (* fast *)
