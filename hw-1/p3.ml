let rec iter (n, f) =
  if n = 0 then (fun x -> x)
  else (fun x -> f ((iter (n - 1, f)) x))

  (* 
(* f should get value, so we use anonymous function. *)

let _ = print_endline(string_of_int (iter(3, (fun x -> 2 + x)) 0))
let _ = print_endline(string_of_int (iter(3, (fun x -> 2 * x)) 1))
let _ = print_endline(string_of_int (iter(3, (fun x -> 2 * x)) 2))
let _ = print_endline(string_of_int (iter(3, (fun x -> 2 * x)) 3))
let _ = print_endline(string_of_int (iter(3, (fun x -> 2 * x)) 4))
let _ = print_endline(string_of_int (iter(3, (fun x -> 2 * x)) 5))
let _ = print_endline(string_of_int (iter(3, (fun x -> 2 * x)) 6))
(* if n < 1 *)
let _ = print_endline(string_of_int (iter(0, (fun x -> 2 * x)) 1)) *)