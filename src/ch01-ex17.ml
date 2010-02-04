let double n = n + n

let halve n = n / 2

let is_even n = n mod 2 = 0

let rec fast_mul a b =
  match b with
  | 0 -> 0
  | 1 -> a
  | _ when is_even b -> fast_mul (double a) (halve b)
  | _ -> a + (fast_mul a (b - 1))
