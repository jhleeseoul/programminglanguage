
type treasure = StarBox | NameBox of string

type key = Bar | Node of key * key

type map =
  | End of treasure
  | Guide of string * map
  | Branch of map * map

exception Error of string

(* Pretty printer for keys *)
let rec string_of_key k =
  match k with
  | Bar -> "Bar"
  | Node (k1, k2) -> "Node(" ^ string_of_key k1 ^ ", " ^ string_of_key k2 ^ ")"

let string_of_keys ks =
  "[" ^ String.concat "; " (List.map string_of_key ks) ^ "]"

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
          with Not_found -> raise (Error ("Unbound key: " ^ x))
        in
        let _ = Node (alpha, shape) in
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

(* Examples 1 ~ 7 *)

let ex1 = End StarBox

let ex2 = End (NameBox "a")

let ex3 = Guide ("a", End (NameBox "a"))

let ex4 = Guide ("a", Guide ("b", End (NameBox "b")))

let ex5 =
  Branch (
    Guide ("a", End (NameBox "a")),
    Guide ("b", End (NameBox "b"))
  )

let ex6 =
  Guide ("a",
    Branch (
      Guide ("b", End (NameBox "b")),
      End StarBox
    )
  )

let ex7 =
  Branch (
    Guide ("a", End (NameBox "a")),
    End StarBox
  )

let () =
  let run_test name m =
    try
      let ks = getReady m in
      Printf.printf "%s => %s\n" name (string_of_keys ks)
    with Error msg ->
      Printf.printf "%s => Error: %s\n" name msg
  in
  run_test "ex1" ex1;
  run_test "ex2" ex2;
  run_test "ex3" ex3;
  run_test "ex4" ex4;
  run_test "ex5" ex5;
  run_test "ex6" ex6;
  run_test "ex7" ex7
