let is_even n = n mod 2 = 0

let square n = n * n

let fast_expt b n =
  let rec expt_iter a b n =
    if n = 0 then
      a
    else if is_even n then
      expt_iter a (square b) (n / 2)
    else
      expt_iter (a * b) b (n - 1)
  in
    expt_iter 1 b n
