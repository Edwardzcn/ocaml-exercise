exception Unexpected            (* character not expected *)
exception Unbalanced            (* str not balanced *)


let balanced_brackets (str: string) : bool =
  try
    let map_func (input_char: char) : char =
      match input_char with
      | '}' -> '{'
      | ')' -> '('
      | '>' -> '<'
      | ']' -> '['
      | _ -> raise Unexpected
    in
    let stk = Stack.create ()
    in
    let iter_func (input_char: char) =  
      match input_char with
      | '{' | '(' | '<' | '[' -> Stack.push input_char stk
      | '}' | ')' | '>' | ']' ->
        (* check first *)
        (* (
         *   if not ( Stack.is_empty stk ) then raise Unbalanced
         *   else
         *     let tmp = Stack.pop stk in
         *     if map_func input_char <> tmp then raise Unbalanced
         * ) *)
        (
          try
            let tmp = Stack.pop stk in
            if map_func input_char <> tmp then raise Unbalanced
          with Stack.Empty -> raise Unbalanced
        )

      | _ -> raise Unexpected
    in
    let () =  String.iter iter_func str
    in
    if Stack.is_empty stk then true
    else raise Unbalanced
  with
  | Unbalanced -> false
  | a -> raise a;;


balanced_brackets "[(]{)}";;
balanced_brackets "[](){[()]}";;
balanced_brackets "[]]";;
balanced_brackets "(";;
