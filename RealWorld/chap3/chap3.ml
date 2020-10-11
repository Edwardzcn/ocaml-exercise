(* Chap3 *)

[1;2;3];;

open Core.Std;;
#require "core_bench"
open Core_bench.Std;;
let empty_list = [];;
let one_element_list = 3::empty_list;;
let one_element_list = "test"::empty_list;;

(* 使用模式从列表抽取数据 *)

let rec drop_value l to_drop =
  match l with
  | []-> []
  | to_drop::tl -> drop_value tl to_drop (* 绑定新的to_drop *)
  | hd::tl -> hd::drop_value tl to_drop;; (* 警告没有使用该match *)

drop_value [1;2;3;4;5;] 5;;     (* 由于非空都走了to_drop::tl 全删除了 *)

let rec drop_value l to_drop =
  match l with
  | [] -> []
  | hd::tl ->
     let new_tl = drop_value tl to_drop in (* 使用常规的if而不是模式匹配来判断 *)
     if hd = to_drop
     then new_tl
     else hd::new_tl
;;
drop_value [1;2;3;4;5;] 5;;

#require "core_bench";;
open Core_bench.Std;;
let rec sum_if l =
  if List.is_empty l then 0
  else List.hd_exn l + sum_if (List.tl_exn l);;

let run_bench tests =           (* 不知道为什么跑步了测试 *)
  Bench.bench
    ~ascii_table:true ~display:Textutils.Ascii_table.Display.column_titles tests;;

let rec sum l =
  match l with
  | [] -> 0
  | hd::tl -> hd + sum tl;;

let numbers = List.range 0 1000 in
    [ Bench.Test.create ~name:"sum_if" (fun () -> ignore (sum_if numbers));
      Bench.Test.create ~name:"sum" (fun () -> ignore (sum numbers))]
    |> run_bench
;;

List.fold ~init:[] ~f:(fun biao x -> x :: biao) [1;2;3;4];; (* List.fold 实验 *)

let max_width header rows =     (* 计算列的最大宽度 *)
  let lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(lengths header)
    ~f:(fun acc row -> List.map2_exn  ~f:Int.max acc (lengths row));;

let render_separator widths =   (* width是数值列表，由上面maxwidth得到 *)
  let pieces = List.map widths
                 ~f:(fun w -> String.make (w+2) '-')
  in
  "|" ^ String.concat ~sep:"+" pieces ^ "|";;

let pad s length = (* 编写代码显示这些数据行 *)
  " " ^ s ^ String.make(length - String.length s + 1) ' ';;
pad "hello" 10;;

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "|" ^ String.concat ~sep:"|" padded ^ "|"
;;
render_row ["Hello";"World"] [10;15];;

let render_table header rows = (* 汇集整个工作建立表格 *)
  let widths = max_width header rows in (* 利用max_width获取梅列宽的widths *)
  String.concat ~sep:"\n"               (* 插入换行分隔符号 *)
    ( render_row header widths          (* 渲染header *)
      :: render_separator widths        (* 渲染分界 *)
    :: List.map rows ~f:(fun row -> render_row row widths) (* 为了渲染每一行，需要再次使用List.map *)
    );;


(* 测试一下最初的例子 *)

printf "\n%s\n"
  (render_table
      [ "language";"architect";"first class"]
      [ ["Lisp";"John McCarthy";"1958"];
        ["C";"Dennis Ritchie";"1969"];
        ["ML";"Robin Milner";"1973"];
        ["OCaml";"Xavier Leroy";"1996"];
  ]);;

List.filter ~f:(fun x -> x mod 3 = 0) [1;2;3;4;5;6;7];;

(* 非尾递归 *)
let rec length = function       (* 无名函数接受一个列表为参数 *)
  | [] -> 0
  | _ :: tl -> 1 + length tl
;;

(* 尾递归 *)
let rec length_plus_n l n =     (* 接受列表和数值两个参数 *)
  match l with
  | [] -> n
  | _ :: tl -> length_plus_n tl (n+1)
;;

let make_list n  = List.init n ~f:(fun x -> x);;
length_plus_n (make_list 500000) 0;;
length (make_list 500000);;

let rec destutter = function
  | [] | [_] as l -> l
  | hd::(hd :: _ as tl) when  hd =hd' -> destutter tl
  | hd:: tl -> hd :: destutter tl
;;
