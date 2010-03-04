let double n = n + n

let halve n = n / 2

let is_even n = n mod 2 = 0

let fast_mul a b =
  let rec mul r a b =
    if a = 0 || b = 0 then
      0
    else if b = 1 then
      r + a
    else if is_even b then
      mul r (double a) (halve b)
    else
      mul (r + a) a (b - 1)
  in
    mul 0 a b
