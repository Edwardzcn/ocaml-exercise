(* Eucalid's algorithm to get gcd *)

let rec gcd x y =               (* b <> 0 *)
  if x mod y = 0 then y else gcd y (x mod y);;

let rec gcd_ref x y =
  if y = 0 then x else gcd_ref y (x mod y);;

gcd 13 27;;
gcd 20536 7826;;
gcd 28 35;;
gcd 14 7;;
gcd 3 0;;

gcd_ref 3 0;;
