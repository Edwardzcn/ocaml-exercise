(* Not understand *)

(* Learn it *)

let rec extract k list =
  if k <= 0 then [[]]
  else match list with
    | [] -> []
    | h :: tl ->
      let with_h = List.map (fun x -> h::x) (extract (k-1) tl)
      in
      let without_h = extract k tl
      in
      with_h @ without_h;;

extract 2 [1;2;3;4;5;6;7;8];;
