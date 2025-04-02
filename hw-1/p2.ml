let rec sigma (a, b, f) =
  if a > b then 0
  else f a + sigma(a + 1, b, f)

(* let test1 = sigma(1, 3, (fun x -> x))
let test2 = sigma(1, 3, (fun x -> x * x))
let test3 = sigma(1, 3, (fun x -> x * x * x))
let test4 = sigma(1, 3, (fun x -> 1))
let test5 = sigma(1, 3, (fun x -> 2))
let test6 = sigma(1, 3, (fun x -> 3))
let test7 = sigma(1, 3, (fun x -> 4))
(* test8 if a > b *)
let test8 = sigma(3, 1, (fun x -> x)) *)

(* let _ = print_endline(string_of_int test1)
let _ = print_endline(string_of_int test2)
let _ = print_endline(string_of_int test3)
let _ = print_endline(string_of_int test4)
let _ = print_endline(string_of_int test5)
let _ = print_endline(string_of_int test6)
let _ = print_endline(string_of_int test7)
let _ = print_endline(string_of_int test8) *)

