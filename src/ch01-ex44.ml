let compose f g = fun x -> f (g x)

let rec repeated f n =
  match n with
  | 1 -> f
  | _ -> compose f (repeated f (n - 1))

let dx = 0.00001

let smooth f =
  fun x ->
    ((f (x -. dx)) +. (f x) +. (f (x +. dx))) /. 3.0

let n_fold_smooth f n = (repeated smooth n) f
