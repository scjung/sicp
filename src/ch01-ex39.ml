let cont_frac n d k =
  let rec term i result =
    if i = 0 then
      result
    else
      term (i - 1) ((n i) /. ((d i) +. result))
  in
    term (k - 1) ((n k) /. (d k))

let square x = x *. x

let tan_cf x k =
  cont_frac
    (fun i -> if i = 1 then x else -.(square x))
    (fun i -> float_of_int ((i * 2) - 1))
    k
