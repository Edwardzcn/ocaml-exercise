let length list  = 
  let rec length_2parm cnt  = function
  | [] -> cnt
  | head::tail -> length_2parm  (cnt+1) tail
  in length_2parm  0 list;;
length [1;2;3;4]                (* test *)
