(*let rec prepend ele num ans =
  if num = 0 then ans else prepend  ele (num-1)  (ele::ans);;*)

let replicate_wrong num =
  let rec elecopy ele num ans =
    match num with
    | 0 -> ans                            (* wrong copy last list*)
    | a -> elecopy ele (num-1) (ele::ans) (* Attention type inference *)
  in
  function
  | [] -> []
  | a::tail -> elecopy a num tail;;

let replicate list  num =
  let rec elecopy ele num ans =
    match num with
    | 0 -> ans
    | a -> elecopy ele (num-1) (ele::ans)
  in
  let rec aux ans = function
    | [] -> ans
    | a::tail -> aux (elecopy a num ans) tail
  in List.rev(aux [] list);;               (* Need reverse *)

replicate   ["a";"b";"c"] 3;;


(*List fold*)

let replicate_fold list  num =
  let rec elecopy num ans ele =
    match num with
    | 0 -> ans
    | a -> elecopy (num-1) (ele::ans) ele
  in
  List.fold_left (elecopy num) [] (List.rev list);;

replicate_fold ["a";"b";"c"] 3;;
