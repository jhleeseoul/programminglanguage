let rec len lst = 
  match lst with
  | [] -> 0
  | x :: xs -> 1 + len xs;;