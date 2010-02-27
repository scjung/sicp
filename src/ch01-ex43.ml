let compose f g = fun x -> f (g x)

let rec repeated f n =
  match n with
  | 1 -> f
  | _ -> compose f (repeated f (n - 1))

