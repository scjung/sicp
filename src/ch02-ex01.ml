let rec gcd a b =
  if b = 0 then
    a
  else
    gcd b (a mod b)

let make_rat n d =
  let g = gcd n d in
  let num = n / g and den = d / g in
    if (den < 0) then
      (-num, -den)
    else
      (num, den)
