let even_occuring_element (xs: 'a list) : 'a =
  let ht = Hashtbl.create (List.length xs)
  in
  let fn fi se acc =
    match se mod 2 with
    | 0 -> fi::acc
    | _ -> acc
  in
  let rec aux = function
  | [] ->
    (* Unfold the hashtable*)
    (match List.rev(Hashtbl.fold fn ht []) with
     | a::_ -> a
     | _ -> failwith "No even-occurring element"
    )
  | hd::tl ->
    (* Change the hashtable *)
    (match Hashtbl.mem ht  hd with
     | false ->
       let _ = Hashtbl.add ht hd 1
       in
       aux tl
     | true ->
       let cnt = (Hashtbl.find ht hd)
       in
       let _ = Hashtbl.replace ht hd (cnt+1)
       in
       aux tl
    )
  in aux xs;;

even_occuring_element [42;42];;

even_occuring_element [1;42;2;2;2;3;42];;
