
type require = id * (cond list)
and cond = Items of gift list
         | Same of id
         | Common of cond * cond
         | Except of cond * cond
and gift = int
and id = A | B | C | D | E

module IdMap = Map.Make(struct type t = id let compare = compare end)
module GiftSet = Set.Make(Int)

exception No_minimum

let rec evalCond cond env =
  match cond with
  | Items gifts -> List.fold_left (fun acc g -> GiftSet.add g acc) GiftSet.empty gifts
  | Same id -> (try IdMap.find id env with Not_found -> GiftSet.empty)
  | Common (c1, c2) -> GiftSet.inter (evalCond c1 env) (evalCond c2 env)
  | Except (c1, c2) -> GiftSet.diff (evalCond c1 env) (evalCond c2 env)

let satisfy reqs env =
  let changed = ref false in
  let new_env =
    List.fold_left (fun acc_env (id, conds) ->
      let required =
        List.fold_left (fun acc c -> GiftSet.union acc (evalCond c env)) GiftSet.empty conds
      in
      let current = try IdMap.find id acc_env with Not_found -> GiftSet.empty in
      let unioned = GiftSet.union current required in
      if GiftSet.equal current unioned then acc_env
      else begin
        changed := true;
        IdMap.add id unioned acc_env
      end
    ) env reqs
  in
  (new_env, !changed)

let rec fixed_point reqs env count =
  if count > 1000 then raise No_minimum
  else
    let (new_env, changed) = satisfy reqs env in
    if changed then fixed_point reqs new_env (count + 1)
    else new_env

let solve reqs =
  fixed_point reqs IdMap.empty 0

let shoppingList reqs =
  let env = solve reqs in
  (* 모든 id를 reqs에서 추출 *)
  let all_ids =
    List.rev (
      List.fold_left (fun acc (id, _) ->
        if List.mem id acc then acc else id :: acc
      ) [] reqs
    )    
  in
  List.map (fun id ->
    let gifts = try IdMap.find id env with Not_found -> GiftSet.empty in
    (id, GiftSet.elements gifts)
  ) all_ids

(* 테스트 예제 *)
let test1 : require list = [
  (A, [ Common (Same B, Same C); Items [1; 2] ]);
  (B, [ Common (Same C, Items [2; 3]) ]);
  (C, [ Items [1]; Except (Same A, Items [3]) ]);
  (D, [ Items []]);
]

let test2 : require list = [
  (A, [ Except (Items [1; 2], Same B) ]);
  (B, [ Except (Same A, Same C) ]);
  (C, [ Common (Items [2], Same A) ])
]

let test3 : require list = [
  (A, [ Items [1] ]);
  (B, [ Items [2] ]);
  (C, [ Items [3] ])
]

let test4 : require list = [
  (A, [ Same B ]);
  (B, [ Same A ])
]

let test5 : require list = [
  (A, [ Items [1;2] ]);
  (B, [ Same A ]);
  (C, [ Same A ])
]

let test6 : require list = [
  (A, [ Common (Items [1;2;3], Items [2;3]) ]);
  (B, [ Except (Items [1;2;3], Items [1]) ])
]

let test7 : require list = [
  (A, [ Same B; Items [1] ]);
  (B, [ Same A; Items [2] ])
]

let test8 : require list = [
  (A, [ Same B ]);
  (B, [ Same C ]);
  (C, [ Same D ]);
  (D, [ Same E ]);
  (E, [ Except (Same A, Items [1]) ])
]

let test9 : require list = [
  (A, [ Items [1]; Common (Same B, Items [1;2]) ]);
  (B, [ Items [1;2] ])
]

let test10 : require list = [
  (A, [ Items [1] ]);
  (B, [ Except (Same A, Items [1]) ]);
  (C, [ Same A; Same B ])
]


let () =
  let result = shoppingList test1 in
  let string_of_id = function
    | A -> "A" | B -> "B" | C -> "C" | D -> "D" | E -> "E"
  in
  let string_of_gift_list lst =
    if lst = [] then "nil"
    else "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"
  in
  let string_of_pair (id, gifts) =
    "(" ^ string_of_id id ^ ", " ^ string_of_gift_list gifts ^ ")"
  in
  let output =
    "[" ^ String.concat "; " (List.map string_of_pair result) ^ "]"
  in
  print_endline output

