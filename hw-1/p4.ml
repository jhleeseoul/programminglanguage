type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

(*tourna -> string*)
let rec parenize tn =
  match tn with
  | LEAF team -> (
      match team with
      | Korea -> "Korea" | France -> "France" | Usa -> "Usa" | Brazil -> "Brazil"
      | Japan -> "Japan" | Nigeria -> "Nigeria" | Cameroon -> "Cameroon"
      | Poland -> "Poland" | Portugal -> "Portugal" | Italy -> "Italy"
      | Germany -> "Germany" | Norway -> "Norway" | Sweden -> "Sweden"
      | England -> "England" | Argentina -> "Argentina"
    )
  | NODE (left, right) -> 
      "(" ^ (parenize left) ^ " " ^ (parenize right) ^ ")"

(* TEST
let _ = print_endline(parenize (NODE (LEAF Korea, LEAF France)))
let _ = print_endline(parenize (NODE (NODE (LEAF Korea, LEAF France), LEAF Usa)))
let _ = print_endline(parenize (NODE (LEAF Korea, NODE (LEAF France, LEAF Usa))))
let _ = print_endline(parenize (NODE (NODE (LEAF Korea, LEAF France), NODE (LEAF Usa, LEAF Brazil))))
let _ = print_endline(parenize (NODE (NODE (LEAF Korea, LEAF France), NODE (NODE (LEAF Usa, LEAF Brazil), LEAF Japan))))
let _ = print_endline(parenize (NODE (NODE (LEAF Korea, LEAF France), NODE (NODE (LEAF Usa, LEAF Brazil), NODE (LEAF Japan, LEAF Nigeria)))))
let _ = print_endline(parenize (NODE (NODE (LEAF Korea, LEAF France), NODE (NODE (LEAF Usa, LEAF Brazil), NODE (NODE (LEAF Japan, LEAF Nigeria), LEAF Cameroon)))))
let _ = print_endline(parenize (NODE (NODE (LEAF Korea, LEAF France), NODE (NODE (LEAF Usa, LEAF Brazil), NODE (NODE (LEAF Japan, LEAF Nigeria), NODE (LEAF Cameroon, LEAF Poland))))))
let _ = print_endline(parenize (NODE (NODE (LEAF Korea, LEAF France), NODE (NODE (LEAF Usa, LEAF Brazil), NODE (NODE (LEAF Japan, LEAF Nigeria), NODE (NODE (LEAF Cameroon, LEAF Poland), LEAF Portugal))))))
let _ = print_endline(parenize (LEAF Korea)) *)