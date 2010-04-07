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

let sub_interval x y =
  add_interval x
    (make_interval (-. (upper_bound y)) (-. (lower_bound y)))

let div_interval x y =
  if (lower_bound y) <= 0.0 && 0.0 <= (upper_bound y) then
    invalid_arg "Division by zero"
  else
    mul_interval x
      (make_interval (1.0 /. (upper_bound y)) (1.0 /. (lower_bound y)))

let mul_interval x y =
  let lx = lower_bound x and ux = upper_bound x
  and ly = lower_bound y and uy = upper_bound y in
    if ux >= 0.0 then
      if lx >= 0.0 then
        if uy >= 0.0 then
          if ly >= 0.0 then
            make_interval (lx *. ly) (ux *. uy) (* lx,ux,ly,uy >= 0.0 *)
          else
            make_interval (ly *. ux) (ux *. uy) (* lx,ux,uy >= 0.0; ly < 0.0 *)
        else
          make_interval (ux *. ly) (lx *. uy)   (* lx,ux >= 0.0; ly,uy < 0.0 *)
      else
        if uy >= 0.0 then
          if ly >= 0.0 then
            make_interval (lx *. uy) (ux *. uy) (* ux,ly,uy >= 0.0; lx < 0.0 *)
          else
            let p1 = lx *. ly and p2 = lx *. uy (* ux,ly >= 0.0; lx,ly < 0.0 *)
            and p3 = ux *. ly and p4 = ux *. uy in
              make_interval (min_of [p1; p2; p3; p4]) (max_of [p1; p2; p3; p4])
        else
          make_interval (ux *. ly) (lx *. ly)   (* ux >= 0.0; lx,ly,uy < 0.0 *)
    else
      if uy >= 0.0 then
        if ly >= 0.0 then
          make_interval (lx *. uy) (ux *. ly)   (* lx,ux < 0; ly,uy >= 0 *)
        else
          make_interval (lx *. uy) (lx *. ly)   (* lx,ux,ly < 0; uy >= 0 *)
      else
        make_interval (ux *. uy) (lx *. ly)     (* lx,ux,ly,uy < 0 *)
