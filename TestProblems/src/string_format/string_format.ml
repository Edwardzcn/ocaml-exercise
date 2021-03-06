let format (str:string) (xs:string list) : string =
  let rec aux xs i str =
    match xs with
      | [] -> str
      | x::xs ->
        let pattern = Str.regexp ("{" ^ (string_of_int i) ^ "}") in
          aux xs (i + 1) (Str.global_replace pattern x str)
  in aux xs 0 str
