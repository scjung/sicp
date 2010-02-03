let rec pascal i j =
  if (j = 1) || (i = j) then
    1
  else
    (pascal (i - 1) (j - 1)) + (pascal (i - 1) j)
