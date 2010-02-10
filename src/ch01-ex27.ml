(* arithmetic functions for big numbers *)
let (//) = Big_int.div_big_int
let (%%) = Big_int.mod_big_int
let (==) = Big_int.eq_big_int
let (--) = Big_int.sub_big_int
let (++) = Big_int.add_big_int
let ( ** ) = Big_int.mult_big_int
let zero = Big_int.zero_big_int
let one = Big_int.unit_big_int
let two = Big_int.big_int_of_int 2
let (!!) = Big_int.big_int_of_int

let square = Big_int.square_big_int

let is_even n = (n %% two) == zero

let rec expmod base exp m =
  if exp == zero then
    Big_int.unit_big_int
  else if is_even exp then
    (square (expmod base (exp // two) m)) %% m
  else
    (base ** (expmod base (exp -- one) m)) %% m

let is_divides a b = (b mod a) = 0

let next n =
  match n with
  | 2 -> 3
  | _ -> n + 2

let smallest_divisor n =
  let rec find_divisor n test_divisor =
    if (test_divisor * test_divisor) > n then
      n
    else if is_divides test_divisor n then
      test_divisor
    else
      find_divisor n (next test_divisor)
  in
    find_divisor n 2

let is_prime n = n = (smallest_divisor n)

let is_carmichael_num n =
  let n' = !! n in
  let rec test a =
    if a == n' then
      true
    else if (expmod a n' n') == a then
      test (a ++ one)
    else
      false
  in
    (not (is_prime n)) && (test one)
