open Core.Std;;

(* 作用域 非掩盖的例子 *)
let languages = "OCaml,Perl,C++,C";;
let dashed_languages =
  let language_list = String.split languages ~on:','  in String.concat ~sep:"-" language_list
;;
language_list;;


(* 作用域 掩盖外层定义的例子 *)
let languages = "OCaml,Perl,C++,C";;
let dashed_languages =
  let languages = String.split languages ~on:',' in String.concat ~sep:"-" languages
;;
languages;;

(* 模式匹配和let  *)

let (ints,strings) = List.unzip [(1,"one") ; (2,"two"); (3,"three")];;
ints;;
strings;;

(* 函数部分练习 *)

(* 匿名函数 *)

(fun x -> x+1)'a';;   (* 报错，因为从type inference推测x应为int类型 *)

(fun x -> x+1)4;;               (* int = 5 *)

List.map ~f:(fun x -> x+1) [1;2;3] ;; (* 传入另一个函数 *)

let increments = [ (fun x -> x +1) ; (fun x -> x+2) ];; (* 嵌入list *)
let get_head_func func_lists  =
  match func_lists with
  | [] -> (fun x -> 0)
  | hd::tl -> hd
;;
let testfunc = get_head_func increments;;
testfunc 4;;

let plusone = (fun x -> x+1);;
let plusone  x = x+1;;

plusone 2;;

let abs_diff   x  y = abs(x-y);;
let abs_diff =
  (fun x ->
    (fun y ->
      abs (x-y)));;



let my_mod x y =
  if x mod y = 0 then true else false;; (* 与下面柯里化展开的函数等价  *)
let my_mod =
  (fun x -> ( fun y -> if x mod y = 0 then true else false))
;;
let my_mod_3 = my_mod 3;;
my_mod 10 2;;
my_mod 10 3;;
my_mod_3 10;;

let rec find_first_stutter list =
  match list with
  | [] | [_] -> None            (* | []  | [_] 称为或模式 or-pattern 两模式$析取， *)
  (* [] 匹配空列表  [_]匹配单元素列表 *)
  | x :: y :: tl -> if x =y then Some x else find_first_stutter (y::tl)
;;
find_first_stutter [1;2;3;4;5;6;1;2];;
find_first_stutter [1;2;2;3;3;4;4;4;];;


(* 前缀中缀测试 *)
Int.max 3 4;;
3 + 4;;
(+) 3 4;;
List.map ~f:((+) 3) [1;2;3];;
List.map ~f:( 3 + ) [1;2;3];;   (* Error *)



(* 优先级和结合性 *)

Int.max 3 (-4);;

let (|>) x f = f x;;
let path = "/usr/bin:/usr/local/bin:/bin:/sbin";;
String.split ~on:':' path       (* |>是左结合的，从path运算内层到外层始终是字符串 *)
|> List.dedup ~compare:String.compare
|> List.iter ~f:print_endline
;;

let (^>) x f = f x;;
((String.split ~on:':' path
^> List.dedup ~compare:String.compare)
^> List.iter ~f:print_endline)
;;


(* 函数声明函数 *)

let some_or_default default = function
  | Some x -> x
  | None -> default
;;
some_or_default "default" (Some "test");;
some_or_default "test" None;;
List.map ~f:(some_or_default "t") [Some "test";Some "default";None];;
some_or_default 3 (Some 5);;

(* 标签参数 *)

let ratio ~num ~denom = float num /. float denom;;
ratio ~num:3 ~denom:4;;
ratio ~denom:4 ~num:3;;

(* 高阶函数与标签 *)

let apply_to_tuple f (first,second) = f ~first ~second;;   (* 这里f作为参数传入apply *)
let apply_to_tuple2 f (first,second) = f ~second ~first;;   (* 我们改变标签参数列出的顺序 *)
let divide ~fi ~se = fi / se;;
apply_to_tuple divide (3,4);;   (* 可以正常工作 *)
apply_to_tuple2 divide (3,4);;  (* 类别不符 *)

(* 可选参数 *)

let concat ?sep x y =
  let sep = match sep with
    | None -> ""
    | Some x -> x in
  x ^ sep ^ y;;
concat "foo" "bar";;
concat ?sep:(Some "what") "foo" "bar";;
