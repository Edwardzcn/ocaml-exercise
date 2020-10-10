let rec compress = function
  | [] -> []
  | [a] -> [a]
  | a::b::tail ->
    if a = b then  compress (b::tail)
    else a::compress (b::tail);;
compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
