open Core.Std;;

let square x = x * x;;

square 4;;
square (square 2);;
let ratio x y =
  Float.of_int x /. Float.of_int y;;

(* 类型推断  *)
let sum_if_true test first second =
  (if test first then first else 0)+(if test second then second else 0);;


(* 推断泛型类型  *)
let first_if_true test x y =
  if test x then x else y;;

let a_tuple = (3,"tree");;
let b_tuple = (3,"four",5.);;
let x,y,z = b_tuple;;

let languages = ["OCaml","Perl","C"];;
let languages = ["OCaml";"Perl";"C"];;

List.length languages;;

"test"::languages;;
"test"::[];;

(* 下面的声明是等价的  *)
[1;2;3];;
1::(2::(3::[]));;
1::2::3::[];;


(* 列表的模式匹配  *)
let my_favorite_language (my_favorite :: the_rest) =
  my_favorite;;
let my_favorite_language my_favorite  =
  match my_favorite with
  | first :: the_rest -> first
  | [] -> "None";;
my_favorite_language [];;
my_favorite_language ["English";"Math"];;

(* 递归列表函数  *)
let rec sum l =
  match l with
  | [] -> 0                     (* 基本情况 *)
  | hd::tl -> hd + sum tl;;     (* 归纳情况  *)

let rec sum_length l =
  match l with
  | [] -> 0                     (* 基本情况  *)
  | hd :: tl -> String.length hd + sum_length tl;; (* 归纳情况  *)

sum [5;4;3;2];;
sum [];;
sum_length ["What";"tes";"ab";"1"];;
sum_length [];;

let x = 7
    in
    let y =  x*x in
    x+y
;;

type point2d = { x: float ; y: float } ;;

let p_1 = {x=3.;y=4.};;
type test2d = { y: float; x:float};;
let p_2 = {y=4.;x=3.};;
p_1 = p_2;;

(* 非字符双关  *)
let magnitude { x=x_pos;y=y_pos} =
  sqrt (x_pos **2. +. y_pos **2.);;
(* 字符双关写法  *)
let magnitude {x;y} =
  sqrt (x**2. +. y**2.);;

let distance v1 v2 =
  magnitude {x = v1.x -. v2.x ; y = v1.y -. v2.y};;

let p_1 = {x=3.;y=4.}
    in
    let p_2 =  {x=5.2;y=4.4}
    in
    distance p_1 p_2 ;;


type circle_desc = {center: point2d; radius: float};;
type rect_desc = {lower_left: point2d; width: float; height: float};;
type segment_desc = {endpoint1: point2d; endpoint2: point2d};;
type scene_element =
  | Circle of circle_desc
  | Rect of rect_desc
  | Segment of segment_desc
;;

(* match 恰与不同场景匹配  *)
let is_inside_scene_element point scene_element =
  match scene_element with
  | Circle {center;radius} -> distance center point < radius (* 注意这是个比较，最后返回的是bool  *)
  | Rect {lower_left; width; height} ->
     point.x > lower_left.x && point.y > lower_left.y &&
       point.x < lower_left.x +. width  && point.y < lower_left.y +. height
  | Segment {endpoint1;endpoint2} -> false
;;

let numbers = [|1;2;3|];;
numbers.(2) <-4;;            (* .(1)语法用于指示数组的一个元素，<-语法表示修改。从0计数  *)
numbers;;

(* 把一些字段显式声明为可变字段，下面例子用于存储对一个数字集合的统计汇总  *)

type running_sum =
  {
    mutable sum: float;
    mutable sum_sq: float;
    mutable samples: int;
  }
;;

let mean rsum = rsum.sum /. float rsum.samples;;
let stdev rsum  =
  sqrt (rsum.sum_sq /. float rsum.samples -. (rsum.sum /. float  rsum.samples)**2.)
;;

