let anagram_detection (parent:string) (child:string) : int =
  (* the sort way *)
  let unfold (input_str:string) : char list =
    let tmp_len = String.length input_str
    in
    let rec aux i acc =
      if i >= tmp_len then List.rev acc
      else
        let charc = input_str.[i] in
        aux (i+1) (charc::acc)
    in
    aux 0 []
  in
  let change_sorted_list (input_str:string) : char list =
    List.sort compare (unfold input_str)
  in
  let child_len = String.length child
  in
  let child_list = change_sorted_list child
  in
  let parent_len = String.length parent
  in
  let check (list1: char list) (list2: char list) : bool =
    (list1 = list2)
  in
  match child_len with
  | 0 -> failwith "child string must be non-empty"
  | _ ->
    let rec aux acc index  =
      if index + child_len  > parent_len then acc
      else match check (change_sorted_list (String.sub parent index child_len ) ) child_list with
        | true -> aux (acc+1) (index+1)
        | false -> aux acc (index+1)
    in
    aux 0 0;;


let anagram_detection_2 (parent:string) (child:string) : int =
  (* try to accelerate the anagram_detection algorithm *)
  (* TODO *)
  1;;

anagram_detection "dAndnaA" "dAn";;
