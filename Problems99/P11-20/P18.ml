(*Extract a slice from a list*)

let slice list i j =
  let rec aux cnt ans = function
    | [] -> List.rev ans
    | head::tail ->
      if cnt < i then aux (cnt+1) ans tail
      else if cnt > j then List.rev ans
      else aux (cnt+1) (head::ans) tail
  in
  aux 0 [] list;;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;

slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 5 3;;

(*Reference*)

let rec fold_until f acc n = function (* no need to go through all the  *)
  | [] -> (acc, [])                   (* pattern matching (first,left) *)
  | h :: t as l -> if n = 0 then (acc, l)
    else fold_until f (f acc h) (n-1) t;; (* iter once and constract new param *)

let slice_ref list i j =
  let _,get = fold_until (fun _ _ -> []) [] i list
  in
  let take,_ = fold_until (fun acc h  -> h::acc) [] (j-i+1) get
  in List.rev take;;
slice_ref ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;

slice_ref ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 5 3;; (* Some Error? *)
