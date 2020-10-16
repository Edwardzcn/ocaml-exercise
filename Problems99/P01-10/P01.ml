let rec last list =
  match list with
  | [] ->  None
  | [x] -> Some x               (* 为什么不能用[_] *)
  | head::tail -> last tail
;;


last [2;4;5];;
