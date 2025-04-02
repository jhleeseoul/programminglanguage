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

let rec crazy3add (c1, c2) =
  match (c1, c2) with
  | (NIL, x) | (x, NIL) -> x 
  | (ZERO f1, ZERO f2) -> ZERO (crazy3add (f1, f2))
  | (ZERO f1, ONE f2) | (ONE f1, ZERO f2) -> ONE (crazy3add (f1, f2))
  | (ZERO f1, MONE f2) | (MONE f1, ZERO f2) -> MONE (crazy3add (f1, f2))
  | (ZERO f1, TWO f2) | (TWO f1, ZERO f2) -> TWO (crazy3add (f1, f2))
  | (ZERO f1, MTWO f2) | (MTWO f1, ZERO f2) -> MTWO (crazy3add (f1, f2))
  | (ONE f1, ONE f2) -> TWO (crazy3add (f1, f2))
  | (MONE f1, MONE f2) -> MTWO (crazy3add (f1, f2))
  | (ONE f1, MONE f2) | (MONE f1, ONE f2) | (TWO f1, MTWO f2) | (MTWO f1, TWO f2) -> ZERO (crazy3add (f1, f2))
  | (TWO f1, ONE f2) | (ONE f1, TWO f2) -> ZERO (crazy3add (crazy3add (f1, f2), ONE NIL))
  | (MTWO f1, MONE f2) | (MONE f1, MTWO f2) -> ZERO (crazy3add (crazy3add (f1, f2), MONE NIL))
  | (TWO f1, TWO f2) -> ONE (crazy3add (crazy3add (f1, f2), ONE NIL))
  | (MTWO f1, MTWO f2) -> MONE (crazy3add (crazy3add (f1, f2), MONE NIL))
  | (TWO f1, MONE f2) | (MONE f1, TWO f2) -> ONE (crazy3add (f1, f2))
  | (MTWO f1, ONE f2) | (ONE f1, MTWO f2) -> MONE (crazy3add (f1, f2))

(* 
(* test crazy3add *)
let _ = print_endline(string_of_int (crazy3val (crazy3add (NIL, NIL))))
let _ = print_endline(string_of_int (crazy3val (crazy3add (ZERO NIL, NIL))))
let _ = print_endline(string_of_int (crazy3val (crazy3add (ONE NIL, NIL))))
let _ = print_endline(string_of_int (crazy3val (crazy3add (MONE NIL, NIL))))
let _ = print_endline(string_of_int (crazy3val (crazy3add (TWO NIL, NIL))))
let _ = print_endline(string_of_int (crazy3val (crazy3add (MTWO NIL, NIL))))
let _ = print_endline(string_of_int (crazy3val (crazy3add (ZERO (ZERO NIL), ZERO NIL))))
let _ = print_endline(string_of_int (crazy3val (crazy3add (ONE (ZERO NIL), ZERO NIL))))
let _ = print_endline(string_of_int (crazy3val (crazy3add (MONE (ZERO NIL), ZERO NIL))))
let _ = print_endline(string_of_int (crazy3val (crazy3add (ZERO (MTWO (ONE NIL)), ZERO NIL))))
let _ = print_endline(string_of_int (crazy3val (crazy3add (ONE (TWO (ZERO (MONE NIL))), ZERO NIL))))
let _ = print_endline(string_of_int (crazy3val (crazy3add (ONE (TWO (ZERO (MONE NIL))), ONE (ZERO NIL)))))
(*testcase : 1 2 1 -1 + 2 2 2*)
let _ = print_endline(string_of_int (crazy3val (crazy3add (ONE (TWO (ONE (MONE NIL))), TWO (TWO (TWO NIL)))))) *)