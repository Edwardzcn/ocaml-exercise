(* Compare the two methods of caculating euler's totient function *)
#use "topfind";;
#require "unix";;


let phi_improved num =
  let factors n =
    let rec aux d n = 
      if n = 1 then [] else
      if n mod d = 0 then
        match aux d (n/d) with
        | (h,n)::t when h = d -> (h,n+1)::t
        | rest -> (d,1)::rest
      else
        aux (d+1) n
    in
    aux 2 n
  in
  let rec pow_tail pi mi ans =
    if mi < 1 then ans else pow_tail pi (mi-1) (pi*ans)
    (* if mi = 1 then mi then pow_tail pi (mi-1) (pi*ans) *) (*not good*)
  in
  List.fold_left (fun sum (a,b) -> sum*(a-1)*(pow_tail a (b-1) 1)) 1 (factors num);;

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


let timeit f a =
  let t0 = Unix.gettimeofday()
  in ignore(f a);
  let t1 = Unix.gettimeofday()
  in t1 -. t0;;

timeit phi 10090;;
timeit phi_improved 10090;;
