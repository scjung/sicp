let runtime = Unix.gettimeofday

(* arithmetic functions for big numbers *)
let (//) = Big_int.div_big_int
let (%%) = Big_int.mod_big_int
let (==) = Big_int.eq_big_int
let (--) = Big_int.sub_big_int
let ( ** ) = Big_int.mult_big_int
let zero = Big_int.zero_big_int
let (!!) = Big_int.big_int_of_int

let square = Big_int.square_big_int

let is_even n = (n %% (!! 2)) == zero

let rec expmod base exp m =
  if exp == zero then
    Big_int.unit_big_int
  else if is_even exp then
    (square (expmod base (exp // (!! 2)) m)) %% m
  else
    (base ** (expmod base (exp -- (Big_int.unit_big_int)) m)) %% m

let fermat_test n =
  let n' = !! n in
  let try_it a =
    let a' = !! a in
      (expmod a' n' n') == a'
  in
    try_it (1 + (Random.int (n - 1)))

let rec fast_prime n times =
  if times = 0 then
    true
  else if fermat_test n then
    fast_prime n (times - 1)
  else
    false

let is_prime n =
  Random.self_init ();  (* Initialize the random number generator *)
  fast_prime n 100

let report_prime elapsed_time =
  print_string " *** ";
  print_float elapsed_time

let timed_prime_test n =
  let rec start_prime_test n start_time =
    if is_prime n then
      report_prime ((runtime ()) -. start_time)
  in
    print_newline ();
    print_int n;
    start_prime_test n (runtime ())
