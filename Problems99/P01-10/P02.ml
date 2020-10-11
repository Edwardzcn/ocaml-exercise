let rec last_two list =
match list with
| [] | [_] -> None
| a::b::[] -> Some (a,b)
| a::b::tail -> last_two tail
;;
last_two ["a";"b" ; "c"; "d"];;
last_two ["a"];;
last_two [];;
last_two [1;2];;
