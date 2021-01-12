module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end;;


let inc x = x + 1
let dec x = x - 1

(* Loggable without too much dumplicated code *)
let log (name : string) (f : int -> int) : int -> int * string =
  fun x -> (f x, Printf.sprintf "Called %s on %i; " name x)
;;

let loggable (name : string) (f : int -> int) : int * string -> int *string =
  fun (x, s1) ->
  let (y, s2) = log name f x in
  (y, s1 ^ s2)
;;

(* Use helpers we can implement the logging versions of functions *)
let inc' : int * string -> int * string = loggable "inc" inc;;
let dec' : int * string -> int * string = loggable "dec" dec;;
let (>>) f g x = x |> f |> g;;
let id' : int * string -> int * string =
  inc' >> dec';;
id' (3,"");;

(* Where's the Monad *)

(* Two fundamental ideas in the code *)
(* from `int` to `int * string` is like return  *)
let return (x : int) : int * string =
  (x, "")
(* Here, the trivial effect is create an empty string *)
let (>>=) (m : int * string) (f : int -> int * string) : int * string =
  let (x , s1) = m in
  let (y , s2) = f x in
  (y, s1 ^ s2)
;;

(* Using `return` and `>>=`, re-implement loggable *)
let loggable' (name : string) (f : int -> int) : int * string -> int * string =
  fun m ->
  m >>= fun x ->
  log name f x >>= fun y ->
  return y;;


module Writer : Monad = struct
  type 'a t = 'a * string
  let return x = (x, "")
  let bind m f =
    let (x, s1) = m in
    let (y, s2) = f x in
    (y, s1^s2)
end;;
