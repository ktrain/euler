(* helpful list functions *)

(* return a list of integers by steps of k from i inclusive to j exclusive
 * (tail-recursive) *)
let range_by_k k i j =
  let rec helper n curr =
    if n >= j then List.rev curr
    else helper (n + k) (n::curr)
  in helper i []

(* return a list of all integers from i inclusive to j exclusive *)
let range i j = range_by_k 1 i j

(* sepConcat s strs is the concatenation of all strs with separator s *)
let rec sepConcat sep sl =
   match sl with
     | [] -> ""
     | (h::t) ->
       let f curr next = curr ^ sep ^ next in
       let base = h in
       let l = t in
       List.fold_left f base l

(* stringOfList f strs is the string form of list strs. Uses parameter f to
 * convert the elements of list strs to strings *)
let string_of_list f l = "[" ^ (sepConcat "; " (List.map f l)) ^ "]"

(* converts int list l to a string *)
let string_of_int_list l = string_of_list string_of_int l

(* return the sum of the members of l *)
let rec sum_list l =
  let f curr x = curr + x in
  let base = 0 in
  List.fold_left f base l

(* return the sum of the squares of the natural numbers up to (including) n *)
let sum_squares n =
  let l = range 0 (succ n) in
  let f curr x = curr + (x * x) in
  let base = 0 in
  List.fold_left f base l

(* return the square of the sum of the natural numbers up to (including) n *)
let square_sum n =
  let sum = sum_list (range 0 (succ n)) in
  sum * sum

(* return a list containing the digits of n *)
let digit_list n =
  let rec helper n l =
    if abs(n) < 10 then n::l
    else helper (n / 10) ((n mod 10)::l)
  in helper n []

(* return l with duplicates removed (order not preserved) *)
let removeDuplicates l =
  let rec helper seen rest =
    match rest with
      | [] -> seen
      | x::xs ->
        let seen' = if List.mem x seen then seen else x::seen in
        let rest' = xs in helper seen' rest'
  in
    helper [] l

(* return the largest element of l *)
let max l =
  let rec helper arr curr =
    match arr with
    | [] -> curr
    | x::xs -> if x > curr then helper xs x else helper xs curr
  in helper l 0

