exception Illegalnum
exception Illegalprecision
let byte_format  ?(precision=2) (input_num:int) : string =
  try
    match (input_num,precision) with
    | (a,_) when a < 0 -> raise Illegalnum
    | (_,b) when b < 0 -> raise Illegalprecision
    | _ ->
      let namelist =  [ "B"; "KB"; "MB"; "GB"; "TB" ] in
      let rec aux (n_float: float) (cnt: int) : string=
        if n_float >= 1024. && cnt < 4 then
          aux (n_float /.1024.) (cnt+1)
        else
          let fmt = Scanf.format_from_string ("%." ^ string_of_int precision ^ "f %s") "%f %s" in
          Printf.sprintf fmt n_float (List.nth namelist cnt) in
      aux (float_of_int input_num) 0
  with
  | Illegalnum -> failwith "n < 0"
  | Illegalprecision -> failwith "precision < 0"
;;


byte_format 156833213 ;;
byte_format 8101 ;;
byte_format ~precision:(-3) 12331;;
