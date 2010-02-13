(*
  a.
 *)
let rec product term a next b =
  if a > b then
    1.0
  else
    (term (float_of_int a)) *. (product term (next a) next b)

let inc x = x + 1

let identity x = x

let factorial n = product identity 1 inc n

let is_even n = (int_of_float n) mod 2 = 0

let approx_pi n =
  let term n =
    (n +. (if is_even n then 2.0 else 1.0)) /. (n +. (if is_even n then 1.0 else 2.0))
  in
    4.0 *. (product term 1 inc n)

(*
  b.
 *)
let product_iter term a next b =
  let rec iter a result =
    if a > b then
      result
    else
      iter (next a) (result *. (term a))
  in
    iter a 1.0
