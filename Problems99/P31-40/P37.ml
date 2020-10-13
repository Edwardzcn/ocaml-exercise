(* calculate euler's totient function *)

(* Improved method *)

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

phi_improved 13;;
phi_improved 10;;
