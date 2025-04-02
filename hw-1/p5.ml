type formula =
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

(* implement expr *)
let rec eval_expr e =
  match e with
  | NUM i -> i
  | PLUS (e1, e2) -> eval_expr e1 + eval_expr e2
  | MINUS (e1, e2) -> eval_expr e1 - eval_expr e2

(*implement formula *)
let rec eval f =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT f1 -> not (eval f1)
  | ANDALSO (f1, f2) -> (eval f1) && (eval f2)
  | ORELSE (f1, f2) -> (eval f1) || (eval f2)
  | IMPLY (f1, f2) -> (not (eval f1)) || (eval f2)
  | LESS (e1, e2) -> eval_expr e1 < eval_expr e2

  (* 
(* TEST *)
let _ = print_endline(string_of_bool (eval TRUE))
let _ = print_endline(string_of_bool (eval FALSE))
let _ = print_endline(string_of_bool (eval (NOT TRUE)))
let _ = print_endline(string_of_bool (eval (NOT FALSE)))
let _ = print_endline(string_of_bool (eval (ANDALSO (TRUE, TRUE))))
let _ = print_endline(string_of_bool (eval (ANDALSO (TRUE, FALSE))))
let _ = print_endline(string_of_bool (eval (ANDALSO (FALSE, TRUE))))
let _ = print_endline(string_of_bool (eval (ANDALSO (FALSE, FALSE))))
let _ = print_endline(string_of_bool (eval (ORELSE (TRUE, TRUE))))
let _ = print_endline(string_of_bool (eval (ORELSE (TRUE, FALSE))))
let _ = print_endline(string_of_bool (eval (ORELSE (FALSE, TRUE))))
let _ = print_endline(string_of_bool (eval (ORELSE (FALSE, FALSE))))
let _ = print_endline(string_of_bool (eval (IMPLY (TRUE, TRUE))))
let _ = print_endline(string_of_bool (eval (IMPLY (TRUE, FALSE))))
let _ = print_endline(string_of_bool (eval (IMPLY (FALSE, TRUE))))
let _ = print_endline(string_of_bool (eval (IMPLY (FALSE, FALSE))))
let _ = print_endline(string_of_bool (eval (LESS (NUM 1, NUM 2))))
let _ = print_endline(string_of_bool (eval (LESS (NUM 1, NUM 1))))
let _ = print_endline(string_of_bool (eval (LESS (NUM 1, NUM 0))))
let _ = print_endline(string_of_bool (eval (LESS (PLUS (NUM 1, NUM 2), NUM 4))))
let _ = print_endline(string_of_bool (eval (LESS (PLUS (NUM 1, NUM 2), NUM (-1))))) *)
