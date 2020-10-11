let rec at list ~index =
match list with
| [] -> None
| head::tail -> if index = 1 then Some head else at tail (index-1)
;;
at  ["a" ; "b" ; "c" ; "d" ; "e"] ~index:3;;
at ~index:5 ["a" ; "b" ; "c" ;"d" ; "e"] ;;

(* test List.nth  *)
List.nth [1;2;4;5;10] 4;;       (* From 0, index 5 will throw an error *)
