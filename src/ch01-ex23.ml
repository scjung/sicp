let runtime = Unix.gettimeofday

let square n = n * n

let is_divides a b = (a mod b) = 0

let next n =
  match n with
  | 2 -> 3
  | _ -> n + 2

let smallest_divisor n =
  let rec find_divisor n test_divisor =
    if (square test_divisor) > n then
      n
    else if is_divides test_divisor n then
      test_divisor
    else
      find_divisor n (next test_divisor)
  in
    find_divisor n 2

let is_prime n = n = (smallest_divisor n)

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

let rec search_for_primes n m =
  if n > m then
    print_newline ()
  else if n mod 2 = 0 then
    search_for_primes (n + 1) m
  else begin
    timed_prime_test n;
    search_for_primes (n + 1) m
  end
