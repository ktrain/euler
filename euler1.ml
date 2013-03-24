(* problem solvers for problems 1,2,6,5,20,16,8, and 48 *)

#use "list.ml"
#use "math.ml"
#use "bigs.ml"

(* problem 1:
 * return the sum of all multiples of 3 or 5 which are less than n (1000) *)
let p1 n =
  let rec helper sum i =
    if i >= n then sum
    else if (i mod 3 = 0) || (i mod 5 = 0) then helper (sum + i) (i + 1)
    else helper sum (i + 1)
  in helper 0 0


(* problem 2:
 * return the sum of all even-valued terms in the Fibonacci sequence which are
 * less than n (4000000) *)
let p2 n =
  let rec helper sum (fib1,fib2) =
    let fib = fib1 + fib2 in
    if fib > n then sum
    else if fib mod 2 = 0 then helper (sum + fib) (fib,fib1)
    else helper sum (fib,fib1)
  in helper 2 (2,1)


(* problem 6:
 * return the difference of the sum of the squares and the square of the sum of
 * the first n natural numbers (100) *)
let p6 n = square_sum n - sum_squares n


(* problem 5:
 * return the smallest number which is evenly divisible by all of the numbers
 * from 1 to n (inclusive) (20) *)
let p5 n =
  let l = range 2 (succ n) in
  let f curr x = lcm curr x in
  let base = 1 in
  List.fold_left f base l


(* problem 20:
 * return the sum of the digits of n! (100) *)
let p20 n = sum_list (bigFac n)


(* problem 16:
 * return the sum of the digits of the number n to the p (2 1000) *)
let p16 p n = sum_list (bigPow p n)


(* problem 8:
 * return the greatest product of five consecutive digits in this 1000-digit
 * number *)
let p8 l =
  let rec helper max xs =
    match xs with
    | x1::x2::x3::x4::x5::xs' ->
        let rem = (x2::x3::x4::x5::xs') in
        let prod = x1 * x2 * x3 * x4 * x5 in
        if prod > max then helper prod rem
        else helper max rem
    | _ -> max
  in helper 0 l

let n8 = [7;3;1;6;7;1;7;6;5;3;1;3;3;0;6;2;4;9;1;9;2;2;5;1;1;9;6;7;4;4;2;6;5;7;
4;7;4;2;3;5;5;3;4;9;1;9;4;9;3;4;9;6;9;8;3;5;2;0;3;1;2;7;7;4;5;0;6;3;2;6;2;3;9;
5;7;8;3;1;8;0;1;6;9;8;4;8;0;1;8;6;9;4;7;8;8;5;1;8;4;3;8;5;8;6;1;5;6;0;7;8;9;1;
1;2;9;4;9;4;9;5;4;5;9;5;0;1;7;3;7;9;5;8;3;3;1;9;5;2;8;5;3;2;0;8;8;0;5;5;1;1;1;
2;5;4;0;6;9;8;7;4;7;1;5;8;5;2;3;8;6;3;0;5;0;7;1;5;6;9;3;2;9;0;9;6;3;2;9;5;2;2;
7;4;4;3;0;4;3;5;5;7;6;6;8;9;6;6;4;8;9;5;0;4;4;5;2;4;4;5;2;3;1;6;1;7;3;1;8;5;6;
4;0;3;0;9;8;7;1;1;1;2;1;7;2;2;3;8;3;1;1;3;6;2;2;2;9;8;9;3;4;2;3;3;8;0;3;0;8;1;
3;5;3;3;6;2;7;6;6;1;4;2;8;2;8;0;6;4;4;4;4;8;6;6;4;5;2;3;8;7;4;9;3;0;3;5;8;9;0;
7;2;9;6;2;9;0;4;9;1;5;6;0;4;4;0;7;7;2;3;9;0;7;1;3;8;1;0;5;1;5;8;5;9;3;0;7;9;6;
0;8;6;6;7;0;1;7;2;4;2;7;1;2;1;8;8;3;9;9;8;7;9;7;9;0;8;7;9;2;2;7;4;9;2;1;9;0;1;
6;9;9;7;2;0;8;8;8;0;9;3;7;7;6;6;5;7;2;7;3;3;3;0;0;1;0;5;3;3;6;7;8;8;1;2;2;0;2;
3;5;4;2;1;8;0;9;7;5;1;2;5;4;5;4;0;5;9;4;7;5;2;2;4;3;5;2;5;8;4;9;0;7;7;1;1;6;7;
0;5;5;6;0;1;3;6;0;4;8;3;9;5;8;6;4;4;6;7;0;6;3;2;4;4;1;5;7;2;2;1;5;5;3;9;7;5;3;
6;9;7;8;1;7;9;7;7;8;4;6;1;7;4;0;6;4;9;5;5;1;4;9;2;9;0;8;6;2;5;6;9;3;2;1;9;7;8;
4;6;8;6;2;2;4;8;2;8;3;9;7;2;2;4;1;3;7;5;6;5;7;0;5;6;0;5;7;4;9;0;2;6;1;4;0;7;9;
7;2;9;6;8;6;5;2;4;1;4;5;3;5;1;0;0;4;7;4;8;2;1;6;6;3;7;0;4;8;4;4;0;3;1;9;9;8;9;
0;0;0;8;8;9;5;2;4;3;4;5;0;6;5;8;5;4;1;2;2;7;5;8;8;6;6;6;8;8;1;1;6;4;2;7;1;7;1;
4;7;9;9;2;4;4;4;2;9;2;8;2;3;0;8;6;3;4;6;5;6;7;4;8;1;3;9;1;9;1;2;3;1;6;2;8;2;4;
5;8;6;1;7;8;6;6;4;5;8;3;5;9;1;2;4;5;6;6;5;2;9;4;7;6;5;4;5;6;8;2;8;4;8;9;1;2;8;
8;3;1;4;2;6;0;7;6;9;0;0;4;2;2;4;2;1;9;0;2;2;6;7;1;0;5;5;6;2;6;3;2;1;1;1;1;1;0;
9;3;7;0;5;4;4;2;1;7;5;0;6;9;4;1;6;5;8;9;6;0;4;0;8;0;7;1;9;8;4;0;3;8;5;0;9;6;2;
4;5;5;4;4;4;3;6;2;9;8;1;2;3;0;9;8;7;8;7;9;9;2;7;2;4;4;2;8;4;9;0;9;1;8;8;8;4;5;
8;0;1;5;6;1;6;6;0;9;7;9;1;9;1;3;3;8;7;5;4;9;9;2;0;0;5;2;4;0;6;3;6;8;9;9;1;2;5;
6;0;7;1;7;6;0;6;0;5;8;8;6;1;1;6;4;6;7;1;0;9;4;0;5;0;7;7;5;4;1;0;0;2;2;5;6;9;8;
3;1;5;5;2;0;0;0;5;5;9;3;5;7;2;9;7;2;5;7;1;6;3;6;2;6;9;5;6;1;8;8;2;6;7;0;4;2;8;
2;5;2;4;8;3;6;0;0;8;2;3;2;5;7;5;3;0;4;2;0;7;5;2;9;6;3;4;5;0]


(* problem 48:
 * return the last ten digits of the series 1^1 + 2^2 + 3^3 + .. + n^n (1000) *)
let p48 n =
  let nums = range 1 (succ n) in
  let rec helper s l =
    match l with
    | [] -> s
    | x::xs -> helper (bigAdd s (bigPow_t 10 x x)) xs
  in List.rev (helper bigZero nums)
(* too inefficient! TO DO: write truncated bigMul, bigPow *)

