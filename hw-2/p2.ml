exception EmptyHeap

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

let rank h =
  match h with
  | EMPTY -> -1
  | NODE(r, _, _, _) -> r

let shake (x, lh, rh) =
  if rank lh >= rank rh then
    NODE(rank rh + 1, x, lh, rh)
  else
    NODE(rank lh + 1, x, rh, lh)

let rec merge (h1, h2) =
  match (h1, h2) with
  | (EMPTY, h) -> h
  | (h, EMPTY) -> h
  | (NODE(_, v1, lh1, rh1), NODE(_, v2, lh2, rh2)) ->
    if v1 <= v2 then
      shake(v1, lh1, merge(rh1, h2))
    else
      shake(v2, lh2, merge(h1, rh2))


let insert (x, h) =
  merge(h, NODE(0, x, EMPTY, EMPTY))

let findMin h =
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_, x, _, _) -> x

let deleteMin h =
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_, x, lh, rh) -> merge(lh, rh)

(* let rec string_of_heap h =
  match h with
  | EMPTY -> "EMPTY"
  | NODE (r, v, lh, rh) ->
    Printf.sprintf "NODE(rank=%d, value=%d, left=%s, right=%s)"
      r v (string_of_heap lh) (string_of_heap rh)

let () =
  Printf.printf "=== Testing merge function ===\n";

  (* Case 1: merge with EMPTY should return the other heap *)
  let h1 = insert (10, EMPTY) in
  assert (merge (h1, EMPTY) = h1);
  assert (merge (EMPTY, h1) = h1);
  Printf.printf "Test 1 (Merge with EMPTY): PASSED\n";

  (* Case 2: Merge two heaps and check min value *)
  let h2 = insert (20, insert (30, EMPTY)) in
  let merged = merge (h1, h2) in
  assert (findMin merged = 10);
  Printf.printf "Test 2 (Merge two heaps, Min = 10): PASSED\n";

  (* Case 3: Merge two heaps with overlapping values *)
  let h3 = insert (15, insert (25, EMPTY)) in
  let merged2 = merge (h2, h3) in
  assert (findMin merged2 = 15);
  Printf.printf "Test 3 (Merge overlapping heaps, Min = 15): PASSED\n";

  (* Case 4: Merge multiple heaps and validate min values *)
  let h4 = insert (5, insert (35, insert (40, EMPTY))) in
  let merged3 = merge (merged, h4) in
  assert (findMin merged3 = 5);
  Printf.printf "Test 4 (Merge multiple heaps, Min = 5): PASSED\n";

  (* Case 5: Print heap structure after merge *)
  Printf.printf "Merged Heap Structure: %s\n" (string_of_heap merged3); *)
