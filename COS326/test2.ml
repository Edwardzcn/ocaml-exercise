type nat = Zero | Succ of nat

let rec nat_to_int (n:nat) : int =
  match n with
  | Zero -> 0
  | Succ n -> 1 + nat_to_int n

let rec double_nat (n:nat) : nat =
  match n with
  | Zero -> Zero
  | Succ n -> Succ (Succ (double_nat n))


let nat_test_zero = Zero

let _ = Printf.printf "%d\n" (nat_to_int (double_nat (Succ Zero) ))
