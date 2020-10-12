(* Determine the prime factors of a given positive integer *)
let factors num =
  let rec gcd x y =
    if y = 0 then x else gcd y (x mod y)
  in
  let coprime x y = gcd x y = 1
  in
  let rec aux n acc  =
    if num mod n = 0 then aux (n-1) (n::acc)
        else aux (n-1) acc;;
  in
  aux num [];;
