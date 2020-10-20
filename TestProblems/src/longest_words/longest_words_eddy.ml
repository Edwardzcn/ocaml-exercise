#load "str.cma"
let longest_words (sentence:string) : string list =
  (* Split sentence on whitespace *)
  let regexp = Str.regexp "[ ]+" in
  let words = Str.split regexp sentence in
  (* Get split words *)
  let hashtable = Hashtbl.create (List.length words) in
  (* Create hashtable save string->int length *)
  let rec getmaxnum acc  = function
    | [] -> acc
    | hd::tl ->
      let len = String.length hd in
      let lower_case = String.lowercase hd in
      (* Add `word` into `ht` only if it's lowercase representation doesn't
             already exist in `ht`. *)
      let () = if not( Hashtbl.mem hashtable lower_case) then Hashtbl.add hashtable lower_case len
      in
      getmaxnum (max acc len) tl
  in
  let maxnum = getmaxnum 0 words
  in
  let fn (fi:string) (se:int) (acc: string list) : string list =
    if se = maxnum then (fi::acc) else acc
  in
  Hashtbl.fold fn hashtable [];;

longest_words "Words words words";;
longest_words "";;
