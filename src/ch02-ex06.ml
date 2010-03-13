let zero = fun f -> fun x -> x

let add_1 n =
  fun f -> fun x -> f ((n f) x)

let one = fun f -> fun x -> f x

let two = fun f -> fun x -> f (f x)

let (++) n1 n2 =
  fun f -> fun x -> (n2 f) ((n1 f) x)
