(* Construct a complete binary tree *)

let rec split_n list acc level_cnt =
  match (level_cnt,list) with
  | (0,_) | (_,[]) -> (List.rev acc, list)
  | (_ , h::t) -> split_n t (h::acc) (level_cnt-1);;

let rec construct  num_list tree_list  acc =
  match (num_list,tree_list) with
  | (p, []) ->  ( List.rev acc) @ (List.map (fun x -> Node(x,Empty,Empty) ) p)
  | (n1::n2 , t1::t2::t3) -> construct n2 t3 (Node(n1,t1,t2) ::acc)
  | (n1::n2,[x]) -> construct n2 [] (Node(n1,x,Empty)::acc)
  | _ ->  invalid_arg "construct error"

  let complete_binary_tree = function
    | [] -> [Empty]
    | lst ->
      let rec aux l = function
         | [] -> []
         | lst -> let p, c = split_n lst [] (1 lsl l)
           in
           construct  p  (aux (l+1) c) []
       in aux 0 lst;;


complete_binary_tree [1;2;3;4;5;6;7;8;9;10];;
