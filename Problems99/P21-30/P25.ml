(*random permutation*)

(*like the random select*)

let rand_permutation list =
  let rec extract cnt acc = function
    | [] -> raise Not_found
    | h::t -> if cnt = 0 then (h,(List.rev acc)@t) else extract (cnt-1) (h::acc) t
  in
  let extract_random list len =
    extract (Random.int len) [] list
  in
  let rec aux n acc list len =  (* Not elegant enough *)
    if n = 0 then acc else
      let pick,rest = extract_random list len
      in aux (n-1) (pick::acc) rest (len-1)
  in
  aux (List.length list) [] list (List.length list);;

rand_permutation [1;2;3;4;5;6;7];;



let rand_permutation_ref list =
  let rec extract cnt acc = function
    | [] -> raise Not_found
    | h::t -> if cnt = 0 then (h,(List.rev acc)@t) else extract (cnt-1) (h::acc) t
  in
  let extract_random list len =
    extract (Random.int len) [] list
  in
  let rec aux  acc list len =  (* Not elegant enough *)
    if len = 0 then acc else
      let pick,rest = extract_random list len
      in aux (pick::acc) rest (len-1)
  in
  aux [] list (List.length list);;

rand_permutation_ref [1;2;3;4;5;6;7;8;9;10];;
