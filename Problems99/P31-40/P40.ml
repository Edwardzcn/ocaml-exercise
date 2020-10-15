let goldbach num =
  let is_prime num =
    let rec is_not_divisor cnt =
      cnt*cnt > num || (num mod cnt <> 0 && is_not_divisor (cnt+1))
    in
    num <> 0 && num <> 1 && is_not_divisor 2
  in
  let rec aux l =
    let test1 = is_prime l
    in
    let test2 = is_prime (num-l)
    in
    if test1 && test2 then (l,num-l)
    else aux (l+1)
  in
  aux 2;;

goldbach 5;;
goldbach 24;;
