(* goldbanch *)
let is_prime num =
  let rec is_not_divisor cnt =
    cnt*cnt > num || (num mod cnt <> 0 && is_not_divisor (cnt+1))
  in
  num <> 0 && num <> 1 && is_not_divisor 2

let goldbach num =
  let rec aux l =
    let test1 = is_prime l
    in
    let test2 = is_prime (num-l)
    in
    if test1 && test2 then (l,num-l)
    else aux (l+1)
  in
  aux 2;;

let range i j =
  let rec aux i j acc =
    if i > j then acc
    else aux (i+1) j (i::acc)
  in
  List.rev(aux i j []);;

range 1 40;;

let goldbach_list i j =
  let rec aux i j acc =
    if i > j then acc
    else match i mod 2 with     (* maybe if-else is better *)
      | 1 -> aux (i+1) j acc
      | 0 -> aux (i+1) j  ((i,goldbach i)::acc)
  in
  List.rev(aux i j []);;

goldbach_list 9 20;;


let goldbach_limit a b lim =
  List.filter (fun (_,(a,b)) -> a>lim && b>lim) (goldbach_list a b);;

goldbach_limit 1 2000 50;;
