(* a.   *)
let cont_frac n d k =
  let rec term i =
    (n i) /. (if i = k then d i else (d i) +. (term (i + 1)))
  in
    term 1

(* b.   *)
let cont_frac_iter n d k =
  let rec term i result =
    if i = 0 then
      result
    else
      term (i - 1) ((n i) /. ((d i) +. result))
  in
    term (k - 1) ((n k) /. (d k))
