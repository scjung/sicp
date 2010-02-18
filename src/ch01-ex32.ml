(* a.   *)
let rec accumulate combiner null_value term a next b =
  if a > b then
    null_value
  else
    combiner (term a) (accumulate combiner null_value term (next a) next b)

let rec product term a next b =
  accumulate ( *.) 1.0 term a next b

let rec sum term a next b =
  accumulate (+.) 0.0 term a next b

(* b.   *)
let accumulate_iter combiner null_value term a next b =
  let rec iter a result =
    if a > b then
      result
    else
      iter (next a) (combiner (term a) result)
  in
    iter a null_value

