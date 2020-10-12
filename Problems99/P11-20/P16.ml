(* Drop every n elements *)

let drop list nth =
  let rec cnt_drop cnt  = function 
    | [] -> []
    | head::tail ->
      if cnt mod nth = 0
      then cnt_drop (cnt+1) tail
      else  head::(cnt_drop (cnt+1) tail)
  in
  cnt_drop 1 list
;;

drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
