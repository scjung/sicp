let double n = n + n

let halve n = n / 2

let is_even n = n mod 2 = 0

let fast_mul a b =
  let rec mul r a b =
    match b with
    | 0 -> 0
    | 1 -> r + a
    | _ when is_even b -> mul r (double a) (halve b)
    | _ -> mul (r + a) a (b - 1)
  in
    mul 0 a b
