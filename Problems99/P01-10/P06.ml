let is_palindrome list =
  List.rev list = list
;;
is_palindrome ["x";"a";"a";"x"];;
is_palindrome ["a";"b"] ;;
is_palindrome ["x"];;
