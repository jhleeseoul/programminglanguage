(* 예외 정의 *)
exception NOMOVE of string

(* 자료형 정의 *)
type item = string
type tree = 
  | LEAF of item
  | NODE of tree list

type zipper = 
  | TOP
  | HAND of tree list * zipper * tree list
  (* HAND(l, up, r)에서
     l : 왼쪽 형제들(elder siblings),
     r : 오른쪽 형제들(younger siblings),
     up : 부모 방향으로의 zipper *)

type location = LOC of tree * zipper
(* 현재 집중 중인 tree와 그 주변 구조 zipper *)

(* 이미 문제에서 예시로 주어진 goLeft 함수 *)
let goLeft loc =
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")
  | LOC(t, HAND(l::left, up, right)) ->
      LOC(l, HAND(left, up, t::right))

(* goRight: 현재 노드의 오른쪽 형제로 이동 *)
let goRight loc =
  match loc with
  | LOC(_, TOP) -> raise (NOMOVE "right of top")
  | LOC(_, HAND(_, _, [])) -> raise (NOMOVE "right of last")
  | LOC(t, HAND(left, up, r::right)) ->
      LOC(r, HAND(t::left, up, right))

(* goUp: 현재 노드의 부모 노드로 이동 *)
let goUp loc =
  match loc with
  | LOC(_, TOP) -> raise (NOMOVE "up of root") 
  | LOC(t, HAND(left, up, right)) ->
      (* 현재 노드 t와 형제들을 묶어서 NODE(...)로 복원 *)
      LOC(NODE (List.rev left @ (t :: right)), up)

(* goDown: 현재 노드가 NODE(...)라면 그 첫 번째 자식으로 이동 *)
let goDown loc =
  match loc with
  | LOC(LEAF _, _) -> raise (NOMOVE "down of leaf")
  | LOC(NODE [], _) -> raise (NOMOVE "down of empty node")
  | LOC(NODE (child::siblings), up) ->
      (* 첫 번째 자식 child로 이동, 나머지는 오른쪽 형제들로 *)
      LOC(child, HAND([], up, siblings))

(* 
(* 예시로 주어진 sampleTree *)
let sampleloc = 
  LOC (LEAF "*", HAND([LEAF "c"], HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []),[LEAF "d"]))
(* test *)
let _ = goLeft sampleloc
let _ = goRight sampleloc
let _ = goUp sampleloc
let _ = goDown sampleloc *)
