let pack lists =
  let rec  auc now_count all_count = function
    | [] -> []
    | [a] ->  (a::now_count)::all_count
    | a::b::tail ->
      if a = b then auc (a::now_count) all_count (b::tail)
      else auc [] ((a::now_count):: all_count) (b::tail)
  in auc [] [] lists;;

pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];; (* why recursive *)

