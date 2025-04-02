type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list


exception InvalidArgument


let rec diff (exp, var) =
  match exp with
  | CONST _ -> CONST 0  (* 상수의 미분은 0 *)
  | VAR x -> if x = var then CONST 1 else CONST 0  (* 변수와 같다면 1, 아니면 0 *)
  | POWER (x, n) ->
      if x = var then TIMES [CONST n; POWER (x, n - 1)] (* n*x^(n-1) *)
      else CONST 0
  | SUM terms ->
      if terms = [] then raise InvalidArgument
      else SUM (List.map (fun e -> diff (e, var)) terms) (* 미분 적용 *)
  | TIMES terms ->
      if terms = [] then raise InvalidArgument
      else
        (* 곱의 미분: (a * b)' = a' * b + a * b' *)
        let rec product_rule lst =
          match lst with
          | [] -> []
          | hd :: tl -> TIMES (diff (hd, var) :: tl) :: (List.map (fun x -> TIMES [hd; x]) (product_rule tl))
        in SUM (product_rule terms)

(* let rec string_of_ae exp =
  match exp with
  | CONST n -> string_of_int n
  | VAR x -> x
  | POWER (x, n) -> x ^ "^" ^ string_of_int n
  | TIMES terms -> "(" ^ String.concat " * " (List.map string_of_ae terms) ^ ")"
  | SUM terms -> "(" ^ String.concat " + " (List.map string_of_ae terms) ^ ")"

let _ = print_endline (string_of_ae (diff (CONST 5, "x")))
let _ = print_endline (string_of_ae (diff (VAR "x", "x")))
let _ = print_endline (string_of_ae (diff (VAR "y", "x")))
let _ = print_endline (string_of_ae (diff (POWER ("x", 3), "x")))
let _ = print_endline (string_of_ae (diff (SUM [POWER ("x", 3); TIMES [CONST 4; VAR "x"]; CONST 7], "x")))
let _ = print_endline (string_of_ae (diff (TIMES [VAR "x"; VAR "y"], "x")))
let _ = print_endline (string_of_ae (diff (TIMES [CONST 5; VAR "x"], "x")))
let _ = print_endline (string_of_ae (diff (SUM [TIMES [CONST 2; POWER ("x", 2)]; TIMES [CONST 3; VAR "x"]; CONST 4], "x")))
let _ = print_endline (string_of_ae (diff (SUM [VAR "x"; VAR "x"; VAR "x"], "x")))
let _ = print_endline (string_of_ae (diff (TIMES [VAR "x"; POWER ("y", 2)], "x")))
let _ = print_endline (string_of_ae (diff (TIMES [], "y"))) *)