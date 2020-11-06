let selection_sort (compare:'a -> 'a -> int) (xs:'a list) : 'a list =
  let rec aux xs =
    match xs with
      | [] -> []
      | x::xs ->
        let rec select acc min xs =
          match xs with
            | [] ->
              (* When we've reached the end of the list, we prepend `min`,
                 and call `aux` on the rest of the list. *)
              min::(aux acc)
            | x::xs when compare x min < 0 ->
              (* `compare` returning a negative value means that `x` is
                 smaller than `min`. So we set the new `min` to `x`. *)
              select (min::acc) x xs
            | x::xs ->
              (* Otherwise, continue looking for a smaller `min` in the rest
                 of the list. *)
              select (x::acc) min xs in
        select [] x xs in
  aux xs;;

selection_sort compare [1;10;3;1;4;5;6;8;2;4;5;1;9];;
