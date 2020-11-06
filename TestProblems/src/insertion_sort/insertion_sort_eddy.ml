let insertion_sort (compare: 'a -> 'a -> int) (xs : 'a list) : 'a list =
  (* Not tail recursive *)
  let rec insert (ele : 'a) (ys : 'a list) : 'a list =
    match ys with
    | [] -> [ele]
    | hd::tl as t when (compare ele hd < 0 || compare ele hd = 0) -> ele::t
    | hd::tl -> hd:: insert ele tl
  in
  (* Tail recursive *)
  let rec aux (xs : 'a list) (ys : 'a list) =
    match xs with
    | [] -> ys
    | hd :: tl -> aux tl (insert hd ys)
  in
  aux xs [];;

insertion_sort compare [1;10;3;1;4;5;6;8;2;4;5;1;9];;
