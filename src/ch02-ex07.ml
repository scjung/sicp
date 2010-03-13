type interval = float * float

let make_interval x y : interval = (x, y)

let lower_bound (i : interval) = fst i

let upper_bound (i : interval) = snd i

let add_interval x y =
  make_interval ((lower_bound x) +. (lower_bound y))

let min_of xs =
  match xs with
  | [] -> invalid_arg "min_of"
  | h :: t -> List.fold_left min h t

let max_of xs =
  match xs with
  | [] -> invalid_arg "max_of"
  | h :: t -> List.fold_left max h t

let mul_interval x y =
  let p1 = (lower_bound x) *. (lower_bound y)
  and p2 = (lower_bound x) *. (upper_bound y)
  and p3 = (upper_bound x) *. (lower_bound y)
  and p4 = (upper_bound x) *. (upper_bound y) in
    make_interval (min_of [p1; p2; p3; p4]) (max_of [p1; p2; p3; p4])

let div_interval x y =
  mul_interval x
    (make_interval (1.0 /. (upper_bound y)) (1.0 /. (lower_bound y)))
