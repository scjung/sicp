let tolerance = 0.00001

let fixed_point f first_guess =
  let is_close_enough v1 v2 = (abs_float (v1 -. v2)) < tolerance in
  let rec try_ n guess =
    let next = f guess in
      print_int n;
      print_string ": ";
      print_float next;
      print_newline ();
      if is_close_enough guess next then
        next
      else
        try_ (n + 1) next
  in
    print_string "1: ";
    print_float first_guess;
    print_newline ();
    try_ 2 first_guess
