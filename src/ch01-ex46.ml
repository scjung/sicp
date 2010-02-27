let tolerance = 0.00001

let average x y = (x +. y) /. 2.0

let square x = x *. x

let iterative_improve is_good_enough improve =
  fun first_guess ->
    let rec try_ guess =
      let next = improve guess in
        if is_good_enough guess next then
          next
        else
          try_ next
    in
      try_ first_guess

let sqrt x =
  let is_good_enough _ guess =
    (abs_float ((square guess) -. x)) < 0.001
  in
  let improve guess = average guess (x /. guess) in
    (iterative_improve is_good_enough improve) 1.0

let fixed_point f first_guess =
  let is_close_enough v1 v2 = (abs_float (v1 -. v2)) < tolerance in
    (iterative_improve is_close_enough f) first_guess
