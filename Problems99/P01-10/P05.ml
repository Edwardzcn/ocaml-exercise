let rev list =                  (* tail_recurv *)
  let rec aux init  = function
    | [] -> init
    | head :: tailist -> aux (head :: init) tailist
  in aux [] list;;


rev ["a";"b";"c"];;

