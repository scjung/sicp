let double n = n + n

let halve n = n / 2

let is_even n = n mod 2 = 0

let rec fast_mul a b =
  if a = 0 || b = 0 then
    0
  else if b = 1 then
    a
  else if is_even b then
    fast_mul (double a) (halve b)
  else
    a + (fast_mul a (b - 1))
