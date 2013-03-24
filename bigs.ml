(* bigNums are lists whose members represent integer digits *)

#use "list.ml"

exception BigParamFailure of string

let bigZero = [0]
let bigOne = [1]
let big n = digit_list n

(* return a list of n x's *)
let rec clone x n =
  if n <= 0
  then []
  else x::clone x (n-1)

(* return (l1, l2), padding the shorter one with leading zeroes *)
let rec padZero l1 l2 =
  let l1_len = List.length l1 in
  let l2_len = List.length l2 in
    if l1_len < l2_len
    then ((clone 0 (l2_len - l1_len))@l1, l2)
    else if l1_len > l2_len
    then (l1, (clone 0 (l1_len - l2_len))@l2)
    else (l1, l2)

(* return l with any leading zeroes removed *)
let rec removeZero l =
  match l with
    | [] -> []
    | h::t -> if h = 0 then removeZero t else h::t

(* return the sum of the bigNums l1 and l2 *)
let bigAdd l1 l2 =
  let add (l1,l2) =
    let f curr h =
      let (cin,result) = curr in
      let (d1,d2) = h in
      let s = d1 + d2 + cin in
      (s / 10, (s mod 10)::result) in
    let base = (0,[]) in
    let args = List.combine (List.rev (0::l1)) (List.rev (0::l2)) in
    let (_,res) = (List.fold_left f base args) in
      res
  in
    removeZero (add (padZero l1 l2))

(* return the product of int i and bigNum l *)
let rec mulByDigit i l =
  if i = 1 then l
  else if i <= 0 then []
  else bigAdd (bigAdd l l) (mulByDigit (i-2) l)

(* return the product of the bigNums l1 and l2 *)
let bigMul l1 l2 =
  let f curr h =
    let (shift,result) = curr in
    let (factor,digit) = h in
    let product = mulByDigit digit (factor @ (clone 0 shift)) in
    (shift + 1, bigAdd result product) in
  let base = (0,[]) in
  let args = List.combine (clone l1 (List.length l2)) (List.rev l2) in
  let (_,res) = (List.fold_left f base args) in
    res

(* return the bigNum representation of n! *)
let bigFac n =
  let r = range 2 (succ n) in
  let rec helper nums l =
    match nums with
    | [] -> l
    | x::xs ->
        let l' = bigMul l (digit_list x) in
        helper xs l'
  in helper r bigOne

(* return the bigNum representation of n to the p *)
let bigPow p n =
  if p < 0 then raise (BigParamFailure "power must be non-negative")
  else let rec helper b x =
    if b = 0 then bigOne
    else if b = 1 then x
    else helper (pred b) (mulByDigit n x)
  in helper p (digit_list n)


(* truncated bigNum operations *)

(* return the last n elements of l *)
let rec last n l =
  let len = List.length l in
  if len <= n then l
  else match l with
  | [] -> []
  | x::xs -> last n xs

(* like bigMul, but truncated to lim digits *)
let bigMul_t lim l1 l2 =
  let f curr h =
    let (shift,result) = curr in
    let (factor,digit) = h in
    let product = mulByDigit digit (factor @ (clone 0 shift)) in
    (shift + 1, bigAdd result product) in
  let base = (0,[]) in
  let args = List.combine (clone l1 (List.length l2)) (List.rev l2) in
  let (_,res) = (List.fold_left f base args) in
    last lim res

(* like bigPow, but truncated to lim digits *)
let bigPow_t lim p n =
  if p < 0 then raise (BigParamFailure "power must be non-negative")
  else let rec helper b x =
    if b = 0 then bigOne
    else if b = 1 then x
    else helper (pred b) (last lim (mulByDigit n x))
  in helper p (digit_list n)
