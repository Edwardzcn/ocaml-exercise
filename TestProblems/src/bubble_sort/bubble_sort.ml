let bubble_sort (compare:'a -> 'a -> int) (xs:'a list) : 'a list =
  let rec bubble xs =
    match xs with
      | [] -> []
      | x::y::xs when (compare x y > 0) ->
        (* `compare` returning a positive value means that `x` is greater than
           `y`. We swap the positions of `x` and `y`, and call `bubble` on the rest of the list. *)
        y::x::(bubble xs)
      | x::xs ->
        (* Otherwise, just call `bubble` on the rest of the list. *)
        x::(bubble xs) in
  let rec aux xs i =
    (* Call `bubble` on the list `n` times, where `n` is the length of the
       list. *)
    if i > 0 then
      let xs = bubble xs in
      let _ = Printf.printf "Inside loop bubble with num= %d | " (i-1) in
      let _ = List.iter (Printf.printf "%d ")  xs in
      let _ = Printf.printf "\n" in
      (* aux (bubble xs) (i - 1) *)
      aux xs (i - 1)
    else
      xs
  in aux xs (List.length xs)
