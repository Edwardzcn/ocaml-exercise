let combine_two_strings (str1:string) (str2:string) (str3:string) :bool =
  let str_to_char (input_str:string) : char list =
    let str_len = String.length input_str in
    let rec aux acc index =
      if index >= str_len then acc
      else
        aux (input_str.[index]::acc) (index+1)
    in
    aux [] 0
  in
  (* may be empty list *)
  let char_lists_combine = List.sort compare ( List.append (str_to_char str1) (str_to_char str2) ) in
  let char_lists_str3 = List.sort compare ( str_to_char str3) in
  if char_lists_str3 = char_lists_combine then true else false;;


combine_two_strings "abc" "cab" "cabcab";;
combine_two_strings "abc" "def" "dabecf";;
combine_two_strings "abc" "cab" "acbbca";;
