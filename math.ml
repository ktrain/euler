(* helpful math functions *)

#use "list.ml"

exception ParamFailure of string

(* return n! *)
let fac n =
  let l = range 2 (succ n) in
  let f curr x = curr * x in
  let base = 1 in
  List.fold_left f base l

(* return the greatest common denominator of a and b *)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

(* return the absolute value of x *)
let abs n = if n < 0 then (-n) else n

(* return the least common multiple of a and b (euclidian algorithm) *)
let lcm a b = (abs (a * b)) / (gcd a b)

(* return n to the power b *)
let pow p n =
  if p < 0 then raise (ParamFailure "power must be non-negative")
  else let rec helper b x =
    if b = 0 then 1
    else if b = 1 then x
    else helper (pred b) (x * n)
  in helper p n
