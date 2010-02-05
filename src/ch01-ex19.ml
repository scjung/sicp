let fib n =
  let rec fib_iter a b p q count =
    if count = 0 then
      b
    else if count mod 2 = 0 then
      fib_iter a b
        ((p * p) + (q * q))
        ((2 * p * q) + (q * q))
        (count / 2)
    else
      fib_iter
        ((b * q) + (a * q) + (a * p))
        ((b * p) + (a * q))
        p
        q
        (count - 1)
  in
    fib_iter 1 0 0 1 n
