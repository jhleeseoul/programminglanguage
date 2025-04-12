type treasure = StarBox | NameBox of string

type key = Bar | Node of key * key

type map =
  | End of treasure
  | Guide of string * map
  | Branch of map * map

exception Error of string
exception IMPOSSIBLE

(* getReady: map -> key list *)
let getReady (m : map) : key list =
  let rec collect m shape table =
    match m with
    | End StarBox -> (table, [Bar])
    | End (NameBox x) ->
        let table' =
          if List.mem_assoc x table then table
          else (x, shape) :: table
        in
        (table', [])
    | Guide (x, m1) ->
        let (table1, keys1) = collect m1 shape table in
        let alpha =
          try List.assoc x table1
          with Not_found -> raise IMPOSSIBLE
        in
        let _ = Node (alpha, shape) in  (* suppress warning by evaluating, even if unused *)
        (table1, keys1)
    | Branch (m1, m2) ->
        let shape1 = Node (Bar, Bar) in
        let (table1, keys1) = collect m1 shape1 table in
        let shape2 = Bar in
        let (table2, keys2) = collect m2 shape2 table1 in
        (table2, keys1 @ keys2)
  in
  let (final_table, base_keys) = collect m Bar [] in
  let name_keys = List.map snd final_table in
  let rec uniq xs =
    match xs with
    | [] -> []
    | x::rest -> if List.mem x rest then uniq rest else x :: uniq rest
  in
  uniq (base_keys @ name_keys)