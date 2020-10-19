let largest_continuous_sum (xs: 'a list) : 'a =
  let fn (all,cnt) x =
    if cnt <= 0 then (max all x ,x) else (max all (cnt + x),(cnt+x))
  in
  let ans,_ = List.fold_left fn (0,0) xs
  in
  ans;;

largest_continuous_sum [1; -7; 32; -2];;
largest_continuous_sum [32; 1; 7; 2];;
