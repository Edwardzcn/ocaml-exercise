let selection_sort (compare: 'a -> 'a -> int) (xs : 'a list) : 'a list =
  (* Tail recursive *)
  let rec select (input : 'a list) (output : 'a list) (min : 'a)   =
    match input with
    | [] -> (min, output)
    | hd :: tl when (compare hd min) < 0 -> select tl (min::output) hd
    | hd :: tl -> select tl (hd::output) min
  in
  (* Tail recursive *)
  let rec aux acc = function
    | [] -> List.rev acc
    | hd :: tl ->
      let a,b = select tl [] hd
      in
      aux (a::acc) b
  in
  aux [] xs;;


selection_sort compare [1;10;3;1;4;5;6;8;2;4;5;1;9];;
