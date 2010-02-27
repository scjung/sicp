let compose f g = fun x -> f (g x)

let rec repeated f n =
  match n with
  | 1 -> f
  | _ -> compose f (repeated f (n - 1))

let tolerance = 0.00001

let average x y = (x +. y) /. 2.0

let fixed_point f first_guess =
  let is_close_enough v1 v2 = (abs_float (v1 -. v2)) < tolerance in
  let rec try_ guess =
    let next = f guess in
      if is_close_enough guess next then
        next
      else
        try_ next
  in
    try_ first_guess

let fixed_point_of_transform g transform guess =
  fixed_point (transform g) guess

let average_damp f =
  fun x -> average x (f x)

let log2 x = (log x) /. (log 2.0)

let nth_root n x =
  fixed_point_of_transform
    (fun y -> (x /. (y ** (float_of_int (n - 1)))))
    (repeated average_damp (int_of_float (log2 (float_of_int n))))
    1.0
