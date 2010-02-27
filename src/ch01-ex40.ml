let square x = x *. x

let cube x = x *. x *. x

let average x y = (x +. y) /. 2.0

let tolerance = 0.00001

let fixed_point f first_guess =
  let is_close_enough v1 v2 = (abs_float (v1 -. v2)) < tolerance in
  let rec try_ n guess =
    let next = f guess in
      if is_close_enough guess next then
        next
      else
        try_ (n + 1) next
  in
    try_ 2 first_guess

let fixed_point_of_transform g transform guess =
  fixed_point (transform g) guess

let average_dump f = fun x -> average x (f x)

let dx = 0.00001

let deriv g =
  fun x -> ((g (x +. dx)) -. (g x)) /. dx

let newton_transform g =
  fun x -> x -. ((g x) /. ((deriv g) x))

let newtons_method g guess =
  fixed_point (newton_transform g) guess

let cubic a b c =
  fun x -> (cube x) +. ((square x) *. a) +. (b *. x) +. c
