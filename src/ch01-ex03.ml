let square x = x * x

let sum_ex x y z =
  if x > y then
    (square x) + (square (if y > z then y else z))
  else
    (square y) + (square (if x > z then x else z))
