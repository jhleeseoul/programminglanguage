let rec merge (lst1,lst2) : int list = match (lst1, lst2) with
  | ([], []) -> []
  | ([], lst2) -> lst2
  | (lst1, []) -> lst1
  | (hd1::tl1, hd2::tl2) -> if hd1 > hd2 then hd1::merge(tl1, lst2)
                            else hd2::merge(lst1, tl2)

(* 
let test1 = merge ([5;3;1], [6;4;2]) = [6;5;4;3;2;1]
let test2 = merge ([3;2;1], [6;5;4]) = [6;5;4;3;2;1]
let test3 = merge ([3;2;1], [3;2;1]) = [3;3;2;2;1;1]
let test4 = merge ([3;2;1], []) = [3;2;1]
let test5 = merge ([], [3;2;1]) = [3;2;1]
let test6 = merge ([], []) = []


let _ = print_endline(string_of_bool test1)
let _ = print_endline(string_of_bool test2)
let _ = print_endline(string_of_bool test3)
let _ = print_endline(string_of_bool test4)
let _ = print_endline(string_of_bool test5)
let _ = print_endline(string_of_bool test6) *)
