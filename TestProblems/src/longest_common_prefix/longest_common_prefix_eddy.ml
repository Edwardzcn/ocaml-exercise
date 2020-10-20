let longest_common_prefix (xs:string list) : string =
  match xs with
  | [] -> failwith "empty list"
  | a::[] -> a
  | a::b ->
    let compare stra strb =
      let lena = String.length stra
      and lenb = String.length strb
      in
      let rec compare_at index =
        if index >= lena || index >= lenb then String.sub stra 0 index
        else if String.get stra index = String.get strb index then compare_at (index+1)
        else
          let _ = print_string (String.sub stra 0 index) 
          in
          String.sub stra 0 index
      in
      compare_at 0
    in
    let rec aux acc  = function
      | [] -> acc
      | hd::tl ->
        let tmp = compare acc hd
        in
        if tmp = "" then ""
        else aux tmp tl 
    in aux a  b;;


longest_common_prefix ["aa";"aa";"ab"];;
longest_common_prefix ["good"; "goo" ; "go"  ];;
