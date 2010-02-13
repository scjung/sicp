let sum term a next b =
  let iter a result =
    if a > b then
      result
    else
      iter (next a) (result +. (term a))
  in
    iter a 0.0
