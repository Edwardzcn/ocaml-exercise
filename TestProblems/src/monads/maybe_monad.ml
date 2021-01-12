(* Monad signature *)

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end;;

(* Example: The Maybe Monad *)

let x = 1 + (4/2);;

let div (x:int) (y:int) : int option =
  if y = 0 then None else Some (x / y);;

(* this won't type check *)
(* let x = 1 + (div 4 2);; *)
(* Error: This expression has type int option
       but an expression was expected of type int *)

(* One solution is to re-code all the existing operators to accept int option. THAT IS TERRIBLE *)

(* Where's the monad *)
let return (x : int) : int option =
  Some x;;

let bind (x: int option) (f: int -> int option) : int option =
  match x with
  | None -> None
  | Some a -> f a
;;

let (>>=) = bind;;

let upgrade op x = x >>= op;;

let upgrade : (int -> int option) -> (int option -> int option) =
  fun op x -> (x >>= op);;

(* Re-implement the arithmetic operations *)

let (+) (x : int option) (y : int option) : int option =
  x >>= (fun a -> (y >>= fun b -> return (Stdlib.(+) a b)));;
(* the same to *)
let (+) (x : int option) (y : int option) : int option =
  x >>= fun a ->
  y >>= fun b ->
  return (Stdlib.(+) a b);;
let (/) (x : int option) (y: int option) : int option =
  x >>= fun a ->
  y >>= fun b ->
  if b = 0 then None else return (Stdlib.(/) a b);;

(* Still a fair amount of dumplication *)

let upgrade_binary op x y =
  x >>= fun a ->
  y >>= fun b ->
  op a b;;

let return_binary op x y =
  return (op x y);;

let ( + ) = upgrade_binary (return_binary Stdlib.( + ));;
let ( - ) = upgrade_binary (return_binary Stdlib.( - ));;
let ( * ) = upgrade_binary (return_binary Stdlib.( * ));;
let div (x:int) (y:int) : int option =
  if y = 0 then None else Some (x Stdlib.(/) y);;
let ( / ) = upgrade_binary div;;
;;

(* The Maybe Monad *)

(* module type Monad = sig
 *   type 'a t
 *   val return : 'a -> 'a t
 *   val bind : 'a t -> ('a -> 'b t) -> 'b t
 * end;; *)

(* An implementation of the monad signature for the maybe monad *)

module Maybe : Monad = struct
  type 'a t = 'a option

  let return x = Some x

  let bind  m f = 
    match m with
    | None -> None
    | Some a -> f a
end;;
