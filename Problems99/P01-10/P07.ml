type 'a node =
  | One of 'a
  | Many of 'a node list;;


let flatten nested_list = 
  let rec aux   = function
    | [] -> []
    | One a::tail  ->  a :: aux tail 
    | Many l::tail  -> aux l @ aux tail
  in aux  nested_list;;
flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;

