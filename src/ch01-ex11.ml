(* recursive process *)
let rec f n =
  if (n < 3) then
    n
  else
    (f (n - 1)) + ((f (n - 2)) * 2) + ((f (n - 3)) * 3)

(* iterative process *)
let f n =
  let rec f_iter counter fn1 fn2 fn3 =
    if (counter > n) then
      fn1
    else
      f_iter (counter + 1)
        (fn1 + (2 * fn2) + (3 * fn3)) fn1 fn2
  in
    if (n < 3) then n else f_iter 3 2 1 0
