exception Illegalnum

let fib_1  (input_num: int) : int =
  try
    match input_num with
    | a when a < 1 -> raise Illegalnum
    | _ ->
      let ht = Hashtbl.create (input_num+1) in
      let _ = Hashtbl.add ht 1 0 in
      let _ = Hashtbl.add ht 2 1 in
      let rec aux cnt =
        if cnt > input_num then Hashtbl.find ht input_num
        else
          let sumup = (Hashtbl.find ht (cnt-1)) + (Hashtbl.find ht (cnt-2)) in
          let _ =  Hashtbl.add ht cnt sumup in
          aux (cnt+1)
      in
      aux 3
  with
    Illegalnum -> failwith "n < 1";;



let fib_2 (input_num: int) : int =
  try
    match input_num with
    | a when a < 1 -> raise Illegalnum
    | 2 -> 1
    | _ ->
      let rec aux last1 last2 cnt =
        if cnt = input_num then (last1 + last2)
        else aux last2 (last1+last2) (cnt+1)
      in
      aux 0 1 3
  with
    Illegalnum -> failwith "n < 1";;

(* (\*hashtable for fib*\)
 * let tbl = Hashtbl.create 1000;;
 * Hashtbl.add tbl 1 1;;
 * Hashtbl.add tbl 2 1;;
 * let rec fib n =
 *   if Hashtbl.mem tbl n then Hashtbl.find tbl n
 *   else
 *     let new1 = fib (n-1) + fib (n-2) in
 *     Hashtbl.add tbl n new1;
 *     new1;; *)

let fib_3 (input_num: int) : int =
  let tbl = Hashtbl.create 1000
  in
  let _ =  Hashtbl.add tbl 1 0 in
  let _ = Hashtbl.add tbl 2 1 in
  let rec aux n =
    if Hashtbl.mem tbl n then Hashtbl.find tbl n
    else
      let new1 = aux (n-1) + aux (n-2) in
      Hashtbl.add tbl n new1;
      new1
  in
  aux input_num;;

let fib_4 n =
  let tbl = Hashtbl.create 10 in
  let () = Hashtbl.add tbl 1 0 in
  let () = Hashtbl.add tbl 2 1 in
  let rec fib n =
    if Hashtbl.mem tbl n then
      Hashtbl.find tbl n
    else
      let fn = fib (n-1) + fib (n-2) in
      Hashtbl.add tbl n fn;
      fn
  in
  fib n;;


(fib_1 10) = (fib_2 10);;
(fib_2 10) = (fib_3 10);;

#load "unix.cma";;
open Unix;;
let timeit f a : float = 
  let t0 = Unix.gettimeofday() in
  ignore(f a);
  let t1 = Unix.gettimeofday() in
  t1 -. t0 ;;

timeit fib_1 3000;;


(* for test tail recursive *)
let unit_test (input_num:int)  =
  (* let f1 = timeit fib_1 input_num in
   * let f2 = timeit fib_2 input_num in
   * let f3 = timeit fib_3 input_num in *)
  Printf.printf "The fib_1 %d cost time = %f\n" input_num (timeit fib_1 input_num);
  Printf.printf "The fib_2 %d cost time = %f\n" input_num (timeit fib_2 input_num);
  (* Printf.printf "The fib_3 %d cost time = %f\n" input_num (timeit fib_3 input_num); *)
  Printf.printf "The fib_4 %d cost time = %f\n" input_num (timeit fib_4 input_num);;

unit_test 100000;;
unit_test 1000000;;
let fib = fib_2;;
