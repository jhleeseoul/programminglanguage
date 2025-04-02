type crazy3 = 
  | NIL
  | ZERO of crazy3
  | ONE of crazy3
  | MONE of crazy3
  | TWO of crazy3
  | MTWO of crazy3

let rec crazy3val f =
  match f with 
  | NIL -> 0
  | ZERO f1 -> crazy3val f1 * 3
  | ONE f1 -> crazy3val f1 * 3 + 1
  | MONE f1 -> crazy3val f1 * 3 - 1
  | TWO f1 -> crazy3val f1 * 3 + 2
  | MTWO f1 -> crazy3val f1 * 3 - 2
(* 
(* test *)
let _ = print_endline(string_of_int (crazy3val NIL))
let _ = print_endline(string_of_int (crazy3val (ZERO NIL)))
let _ = print_endline(string_of_int (crazy3val (ONE NIL)))
let _ = print_endline(string_of_int (crazy3val (MONE NIL)))
let _ = print_endline(string_of_int (crazy3val (TWO NIL)))
let _ = print_endline(string_of_int (crazy3val (MTWO NIL)))
let _ = print_endline(string_of_int (crazy3val (ZERO (ZERO NIL))))
let _ = print_endline(string_of_int (crazy3val (ONE (ZERO NIL))))
let _ = print_endline(string_of_int (crazy3val (MONE (ZERO NIL))))
let _ = print_endline(string_of_int (crazy3val (ZERO(MTWO(ONE NIL)))))
let _ = print_endline(string_of_int (crazy3val (ONE(TWO(ZERO(MONE NIL)))))) *)
