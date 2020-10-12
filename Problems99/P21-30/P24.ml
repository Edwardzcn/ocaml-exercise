(*lotto: drawn n different random numbers from set 1...M*)
let lotto_select_wrong num modnum =   (* Wrong ! *)
  let rec  aux cnt acc =
    if cnt = num then acc else aux (cnt+1) (Random.int modnum :: acc)
  in aux 0 [];;
lotto_select_wrong 6 49;;
lotto_select_wrong 0 45;;

(*using range and random select*)


let lotto_select_right num modnum = 
  let numset = range 1 modnum
  in rand_select numset num;;

lotto_select_right 6 49;;
lotto_select_wrong 10 10;;      (* WRONG *)
lotto_select_right 10 10;;      (* RIGHT *)
