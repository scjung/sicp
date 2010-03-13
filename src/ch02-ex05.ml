let cons a b =
  (2.0 ** a) *. (3.0 ** b)

let rec car n =
  if mod_float n 3.0 = 0.0 then
    car (n /. 3.0)
  else
    (log n) /. (log 2.0)

let rec cdr n =
  if mod_float n 2.0 = 0.0 then
    cdr (n /. 2.0)
  else
    (log n) /. (log 3.0)
