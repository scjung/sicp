let cube x = x *. x *. x

let rec sum term a next b =
  if a > b then
    0.0
  else
    (term a) +. (sum term (next a) next b)

let inc x = x + 1

let simpson_integral f a b n =
  let h = (float_of_int (b - a)) /. (float_of_int n) in
  let term k =
    let r = a mod 3 in
      (match k with
       | 0 -> 1.0
       | _ when k = n -> 1.0
       | _ when (k mod 2) = 0 -> 2.0
       | _ -> 4.0
      ) *. (f (((float_of_int k) *. h) +. (float_of_int a)))
  in
    (h /. 3.0) *. (sum term 0 inc n)
