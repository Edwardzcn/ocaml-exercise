(* Euler totient function *)

(* Primitive method *)

let phi num =
  let rec gcd x y =
    if y = 0 then x else gcd y (x mod y)
  in
  let coprime x y = gcd x y = 1
  in
  let rec aux cnt ans  =
    if cnt = num then ans 
    else if coprime cnt num then aux (cnt+1) (ans+1) else aux (cnt+1) ans
  in
  aux 0 0;;

phi 10;;
phi 13;;

