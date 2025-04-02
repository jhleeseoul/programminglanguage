(*** 1. Queue 모듈 시그니처 선언 ***)
module type Queue = sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ : queue
  val enQ : queue * element -> queue
  val deQ : queue -> element * queue
end

(*** 2. 두 개의 스택(리스트)로 큐를 구현하는 모듈 ***)
module IntListQ : Queue with type element = int list = struct
  type element = int list
  (* queue = (front, back) 쌍으로, front와 back 각각 int list list 형식의 스택을 둠둠. *)
  type queue = element list * element list
  
  exception EMPTY_Q
  
  (* 빈 큐는 두 스택 모두 비어 있는 상태 *)
  let emptyQ : queue = ([], [])

  (* enQ : (queue, element) -> queue
     새 원소 x를 back 스택에 push 하는 방식 *)
  let enQ (((front, back), x): queue * element) : queue =
    (front, x :: back)

  (* deQ : queue -> element * queue
     front가 비어 있지 않다면 front에서 pop,
     front가 비어 있으면 back을 뒤집어 front로 옮겨서 pop *)
  let deQ ((front, back): queue) : element * queue =
    match front with
    | hd :: tl ->
        (* front가 이미 있으면 그냥 pop *)
        (hd, (tl, back))
    | [] ->
        (* front가 비었다면 back을 뒤집어서 새 front를 만든 뒤 pop *)
        match List.rev back with
        | [] -> raise EMPTY_Q
        | hd :: tl -> (hd, (tl, []))
end

(*** 3. 위 모듈을 사용하는 예시 및 테스트 케이스 ***)
(* let _ = 
  (* 큐 생성 *)
  let q = IntListQ.emptyQ in
  (* 큐에 원소 추가 *)
  let q = IntListQ.enQ (q, [5;3;4]) in
  let q = IntListQ.enQ (q, [2]) in
  let q = IntListQ.enQ (q, [3]) in
  (* 큐에서 원소 제거 *)
  let (x, q) = IntListQ.deQ q in
  let (y, q) = IntListQ.deQ q in
  let (z, q) = IntListQ.deQ q in


  
  (* 첫번째 원소 출력력 *)
  print_endline ("디큐된 원소 x는 = [" ^ string_of_int (List.hd x) ^ "]" );
  print_endline ("디큐된 원소 y는 = [" ^ string_of_int (List.hd y) ^ "]" );
  print_endline ("디큐된 원소 z는 = [" ^ string_of_int (List.hd z) ^ "]" );
   *)