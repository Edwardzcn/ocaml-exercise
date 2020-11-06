let merge_sort (compare:'a -> 'a -> int) (xs:'a list) : 'a list =
  let rec merge (xs : 'a list) (ys : 'a list) : 'a list =
    (* merge the two half *)
    match xs,ys with
    | xs' , [] -> xs'
    | [] , ys' -> ys'
    | x::xs' , y::ys' when compare x y < 0 -> x::(merge xs' ys)
    | x::xs' , y::ys' -> y::(merge xs ys')
  in
  let split (input : 'a list) : ('a list * 'a list) =
    let len = (List.length input)/2 in
    (* List sublist *)
    let rec aux xs acc i =
      match xs with
      | x:: xs when i < len -> aux xs (x::acc) (i+1)
      | _ -> List.rev acc, xs in
    (* Here is input not xs, or you will get stackoverflow
    *)
    aux input [] 0
  in
  (* Not tail recursive *)
  let rec aux (xs : 'a list) : 'a list =
    match xs with
    | [] -> [] | [x] -> [x]
    | _ ->
      let subx,suby = split xs in
      merge (aux subx) (aux suby)
  in
  aux xs;;

merge_sort compare [1;10;3;1;4;5;6;8;2;4;5;1;9];;
