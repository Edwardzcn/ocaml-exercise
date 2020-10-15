(*determine whether two positive integer numbers are comrime*)

let comprime x y =              (* not elegant *)
  if gcd x y = 1 then true else false;;

let comprime_ref x y = gcd x y = 1;;
