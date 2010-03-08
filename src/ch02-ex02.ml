let average x y = (x +. y) /. 2.0

type point = float * float

let make_point x y : point = (x, y)

let x_point (p : point) = fst p

let y_point (p : point) = snd p

let print_point p =
  print_newline ();
  print_string "(";
  print_float (x_point p);
  print_string ",";
  print_float (y_point p);
  print_string ")"

type segment = point * point

let make_segment s e : segment = (s, e)

let start_segment (seg : segment) = fst seg

let end_segment (seg : segment) = snd seg

let midpoint_segment seg : point =
  let s = start_segment seg
  and e = end_segment seg in
    make_point
      (average (x_point s) (x_point e))
      (average (y_point s) (y_point e))
