let echo () =
  let i = read_int () + 5 in
  let str = string_of_int i in
  print_endline ("> " ^ str);;