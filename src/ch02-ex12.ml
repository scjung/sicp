type interval = float * float

let make_interval x y : interval = (x, y)

let lower_bound (i : interval) = fst i

let upper_bound (i : interval) = snd i

let make_center_width c w =
  make_interval (c -. w) (c +. w)

let center i =
  ((lower_bound i) +. (upper_bound i)) /. 2.0

let width i =
  ((upper_bound i) -. (lower_bound i)) /. 2.0

let make_center_percent c p =
  make_center_width c (c *. (p /. 100.0))

let percent i =
  ((width i) /. (center i)) *. 100.0
