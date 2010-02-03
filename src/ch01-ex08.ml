let square x = x *. x

let average x y = (x +. y) /. 2.0

let improve guess x = (((square guess) /. x) +. (2.0 *. guess)) /. 3.0

let is_good_enough old_guess guess =
  (abs_float (old_guess -. guess)) < 0.00001

let rec sqrt_iter old_guess guess x =
  if is_good_enough old_guess guess then
    guess
  else
    sqrt_iter guess (improve guess x) x

let sqrt x = sqrt_iter 1.0 (improve 1.0 x) x
