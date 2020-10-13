(* a list of prime numbers *)

let is_prime_ref num =
  let num = abs num in
  let rec is_not_divisor_ref cnt =
    cnt * cnt > num || (num mod cnt <> 0 && is_not_divisor_ref (cnt+1))
  in
  num <> 0 && num <> 1 && is_not_divisor_ref 2;;

let rec  all_primes a b =
  if a>b then [] else
    let rest = all_primes (a+1) b in
    if is_prime_ref a  then a ::rest else rest;;

all_primes 3 1999;;


let rec all_primes a b =
  let range a b 
