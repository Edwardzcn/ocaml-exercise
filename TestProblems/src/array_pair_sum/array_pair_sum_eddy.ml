let array_pair_sum (xs:int list) (sum:int) : (int * int) list =
  let hashtable = Hashtbl.create (List.length xs)
  in
  let fn acc x =
    let swap a b =
      if a < b then (a,b) else (b,a)
    in
    let reminder = sum - x
    in
    match Hashtbl.mem hashtable reminder with
    | true ->
      (match Hashtbl.find hashtable reminder with
      | false ->
        let _ = Hashtbl.replace hashtable reminder true
        in
        let _ = Hashtbl.replace hashtable x true
        in
        (swap x reminder) :: acc
      | true ->
        acc)
    | false ->
      let _ = Hashtbl.replace hashtable x false
      in acc

  in List.fold_left fn [] xs
