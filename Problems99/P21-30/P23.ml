(*random select*)
let rand_select list n =
  let rec extract acc n = function (* extract a number *)
    | [] -> raise Not_found
    | h::t -> if n = 0 then (h,(List.rev acc)@t) else extract (h::acc) (n-1) t
  in
  let extract_rand list len  =  (* random extract *)
    extract [] (Random.int len) list
  in
  let rec aux n acc list len =
    if n = 0 then acc else
      let picked,rest = extract_rand list len
      in
      aux (n-1) (picked::acc) rest (len-1)
  in
  aux n [] list (List.length list);;

rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
