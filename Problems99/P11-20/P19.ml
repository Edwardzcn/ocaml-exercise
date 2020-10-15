let split list num  =
  let rec aux num cnt acc = function
    | [] -> (List.rev acc,[])
    | head::tail ->
      if cnt = num-1 then (List.rev(head::acc) ,tail)
      else aux num (cnt+1) (head::acc) tail
  in aux num 0 [] list;;

let rotate list num =
  let len = List.length list
  in
  let pos_num = if num = 0 then 0 else (len+num) mod len
  in
  if pos_num = 0 then list
  else let a,b = split list pos_num in b@a;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
