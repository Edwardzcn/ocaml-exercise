let missing_element (xs:'a list) (ys:'a list) : 'a =
  (* There is an array of non-negative integers. A second array is formed by shuffling the elements of the first array and deleting a random element. Given these two arrays, find which element is missing in the second array. *)
  let ht = Hashtbl.create (List.length xs) in
  let fn y = Hashtbl.add ht y false in
  let _ = List.iter fn ys in
  let rec aux = function
    | [] -> failwith "invalid input"
    | hd::tl ->
      match Hashtbl.mem ht hd with
      | false -> hd
      | true ->
        let _ = Hashtbl.remove ht hd
        in
        aux tl
  in
  aux xs;;


missing_element [16; 42; 23; 42] [16; 42; 23];;
missing_element [42] [42; 42];;
