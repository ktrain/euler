let count = 200000

let primes =
  let primes = Array.create (max count 3) 0 in
  primes.(0) <- 2; primes.(1) <- 3; primes.(2) <- 5; primes

let rec is_prime i pr bd =
  if primes.(i) > bd then true
  else if pr mod primes.(i) = 0 then false else is_prime (succ i) pr bd

let rec prime_n psize nr tog =
  if psize < count then
    let psize' =
      if is_prime 2 nr (truncate (sqrt (float nr))) then begin
        primes.(psize) <- nr; succ psize end
      else psize in
    prime_n psize' (nr + tog) (6 - tog)

let _ = prime_n 3 7 4; Printf.printf "prime %d: %d\n" count primes.(pred count)
