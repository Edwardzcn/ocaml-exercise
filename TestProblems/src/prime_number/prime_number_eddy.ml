let prime_number (n:int) : bool =
  let n = abs n in
  let rec is_not_divisor d =    (* caculate d*d many times *)
    d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
  n <> 1 && is_not_divisor 2;;
prime_number 7919;;
prime_number 7920;;
