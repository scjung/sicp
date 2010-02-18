let cont_frac n d k =
  let rec term i result =
    if i = 0 then
      result
    else
      term (i - 1) ((n i) /. ((d i) +. result))
  in
    term (k - 1) ((n k) /. (d k))

let approx_e k =
  2.0 +. (cont_frac
            (fun i -> 1.0)
            (fun i ->
               if (i mod 3) = 2 then
                 float_of_int (i - (i / 3))
               else
                 1.0)
            k)
