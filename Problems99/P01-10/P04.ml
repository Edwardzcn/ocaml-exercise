let rec length_2param list index =
match list with
| [] -> index
| head::tail -> length_2param tail (index+1);;
length_2param ["a" ; "b" ; "c"]  0  ;; (* not good *)

(* tail-recursive *)
let length_1param list =
let rec aux list cnt =
match list with
| [] -> cnt
| hd::td -> aux td (cnt+1)
in aux list 0;;
length_2param [1;2;3] 0;;
length_1param [1;2;3];;

