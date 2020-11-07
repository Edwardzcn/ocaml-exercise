(* open Base
 * open Stdio
 * 
 * let build_counts () =
 *   In_channel.fold_lines In_channel.stdin ~init:[] ~f:(fun counts line ->
 *     let count =
 *       match List.Assoc.find ~equal:String.equal counts line with
 *       | None -> 0
 *       | Some x -> x
 * 
 *     in
 *     List.Assoc.add ~equal:String.equal counts line (count + 1)
 *   )
 * 
 * let () =
 *   build_counts ()
 *   |> List.sort ~compare:(fun (_,x) (_,y) -> Int.descending x y)
 *   |> (fun l -> List.take l 10)
 *   |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line) *)

(* Rewrite freq.ml *)

(* With counter.ml we compiled to Counter module, we will change the anonymous fun counts line -> ... to Counter.touch *)

open Base
open Stdio

let build_counts () =
  In_channel.fold_lines In_channel.stdin ~init:[] ~f:Counter.touch

let () =
  build_counts ()
  |> List.sort ~compare:(fun (_,x) (_,y) -> Int.descending x y)
  |> (fun l -> List.take l 10)
  |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line)
