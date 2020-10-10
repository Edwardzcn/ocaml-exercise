(* Chap3 *)

[1;2;3];;

open Core.Std;;

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
