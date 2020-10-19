let integer_difference (n:int) (xs:int list) : int =
  let hashtable = Hashtbl.create (List.length xs)
  in
  let fn x =
    match Hashtbl.mem hashtable x with
    | false -> Hashtbl.add hashtable x 1
    | true -> Hashtbl.replace hashtable x (1 + Hashtbl.find hashtable x)
  in
  let _ = List.iter fn xs       (* List.iter for uint, List.map for 'b list *)
  in
  let fn2 fi se acc =
    let calc x =
      match Hashtbl.mem hashtable x with
      | false -> 0
      | true -> Hashtbl.find hashtable x
    in
    acc+(calc (fi + n))*se
  in
  Hashtbl.fold fn2 hashtable 0;;



integer_difference 4 [1; 1; 5; 6; 9; 16; 27];;
integer_difference 0 [1;2;3];;
integer_difference 3 [1;4;4;1;5;2];;
