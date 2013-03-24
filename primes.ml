(* helpful primes functions *)

#use "list.ml"

(* return the list l modified so that integers which are multiples of the value
 * n are removed *)
(*let removeMultiplesOf n l =
  let rec helper arr curr =
    match arr with
    | [] -> List.rev curr
    | x::xs ->
        if x / 2 < n then helper xs (x::curr)
        else if x mod n = 0 then helper xs curr
        else helper xs (x::curr)
  in helper l []*)

let primes_100 = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53;
  59; 61; 67; 71; 73; 79; 83; 89; 97]

let getNMultiplesOf x n =
  let lim = n + 2 in
  let rec helper i curr =
    if i >= lim then curr
    else helper (succ i) ((i * x)::curr)
  in helper 1 []

let removeMultiplesOf n l =
  let len = (max l) / n in print_endline ("removing multiples of " ^
    string_of_int n);
  let bads = getNMultiplesOf n len in
  let rec helper arr curr =
    match arr with
    | [] -> List.rev curr
    | x::xs ->
        if List.mem x bads then helper xs curr
        else helper xs (x::curr)
  in helper l []

let range_prime j =
  let k = 2 in
  let rec helper n curr =
    if n >= j then List.rev curr
    else if List.mem n bads then helper (n + k) curr
    else helper (n + k) (n::curr)
  in helper 3 []

(* return an array containing all prime numbers below n exclusive. sieve of
 * eratosthenes *)
let eratosthenes n =
  let arr = range_prime n in
  let lim = succ (truncate (sqrt (float n))) in
  let rec helper l curr =
    match l with
    | [] -> 2::3::5::7::11::13::17::19::23::29::31::(List.rev curr)
    | p::xs -> if p < lim then helper (removeMultiplesOf p xs) (p::curr)
        else 2::(List.rev curr)@l
  in helper arr []

