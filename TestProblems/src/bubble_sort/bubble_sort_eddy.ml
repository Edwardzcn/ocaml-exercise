let bubble_sort (compare: 'a-> 'a -> int) (xs: 'a list) : 'a list =
  let len = List.length xs
  in
  (* Not tail recursive *)
  let rec bubble (input : 'a list) (cnt : int) : 'a list =
    if cnt = 0 then
      input                     (* with the sorted tail *)
    else
      match input with
      | [] -> failwith "should be cnt = 0, inside loop\n"
      | x::y::tl  when (compare x y > 0) -> y :: bubble (x::tl) (cnt-1)
      | x::tl -> x ::  bubble tl (cnt-1)
  in
  (* Tail recursive *)
  let rec aux (input : 'a list) (cnt : int) : 'a list =
    if cnt = 0 then input
    else
      (* Notice that we need bubble to change xs. Functional programming only cares about the values *)
      let output = bubble input cnt in
      let _ = Printf.printf "Inside loop bubble with num= %d | " cnt in
      let _ = List.iter (Printf.printf "%d ")  output in
      let _ = Printf.printf "\n" in
      aux output (cnt-1)
  in
  aux xs  len;;


bubble_sort compare [1;10;3;1;4;5;6;8;2;4;5;1;9];;
