let filtered_accumulate combiner filter null_value term a next b =
  let rec iter a result =
    if a > b then
      result
    else
      iter (next a) (if filter a then combiner (term a) result else result)
  in
    iter a null_value

let square n = n * n

let is_divides a b = (b mod a) = 0

let smallest_divisor n =
  let rec find_divisor n test_divisor =
    if (square test_divisor) > n then
      n
    else if is_divides test_divisor n then
      test_divisor
    else
      find_divisor n (test_divisor + 1)
  in
    find_divisor n 2

let is_prime n = n = (smallest_divisor n)

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let inc n = n + 1

let identity n = n

(* a.   *)
let sum_prime_squares a b =
  filtered_accumulate (+) is_prime 0 square a inc b

(* b.   *)
let product_rel_primes n =
  let is_rel_prime i = (gcd i n) = 1 in
    filtered_accumulate ( *) is_rel_prime 1 identity 1 inc (n - 1)
