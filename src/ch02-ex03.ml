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

type rect = (point * point) * (point * point)

let make_rect top_left bottom_right : rect =
  ((top_left, make_point (x_point bottom_right) (y_point top_left)),
   (make_point (x_point top_left) (y_point bottom_right), bottom_right))

let top_left_rect (r : rect) : point = fst (fst r)

let top_right_rect (r : rect) : point = snd (fst r)

let bottom_left_rect (r : rect) : point = fst (snd r)

let bottom_right_rect (r : rect) : point = snd (snd r)

let top_segment_rect r =
  make_segment (top_left_rect r) (top_right_rect r)

let bottom_segment_rect r =
  make_segment (bottom_left_rect r) (bottom_right_rect r)

let left_segment_rect r =
  make_segment (top_left_rect r) (bottom_left_rect r)

let right_segment_rect r =
  make_segment (top_right_rect r) (bottom_right_rect r)

let width_rect r =
  let horizontal = top_segment_rect r in
    (x_point (end_segment horizontal)) -. (x_point (start_segment horizontal))

let height_rect r =
  let vertical = left_segment_rect r in
    (y_point (end_segment vertical)) -. (y_point (start_segment vertical))

let perimeter_rect r =
  ((width_rect r) +. (height_rect r)) *. 2.0

let area_rect r = (width_rect r) *. (height_rect r)
