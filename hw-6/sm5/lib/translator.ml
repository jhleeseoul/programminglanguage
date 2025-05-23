(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * Homework "SM5"
 *)

(* TODO : complete this function *)
let rec trans : K.program -> Machine.command = function
  | K.NUM i -> [ Machine.PUSH (Machine.Val (Machine.Z i)) ]
  | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [ Machine.ADD ]
  | K.LETV (x, e1, e2) ->
      trans e1
      @ [
          Machine.MALLOC;
          Machine.BIND x;
          Machine.PUSH (Machine.Id x);
          Machine.STORE;
        ]
      @ trans e2
      @ [ Machine.UNBIND; Machine.POP ]
  | K.READ x ->
      [
        Machine.GET;
        Machine.PUSH (Machine.Id x);
        Machine.STORE;
        Machine.PUSH (Machine.Id x);
        Machine.LOAD;
      ]
    | K.SUB (e1, e2) ->
      trans e1 @ trans e2 @ [ Machine.SUB ]
  | K.MUL (e1, e2) ->
      trans e1 @ trans e2 @ [ Machine.MUL ]
  | K.DIV (e1, e2) ->
      trans e1 @ trans e2 @ [ Machine.DIV ]
  | K.TRUE ->
      [ Machine.PUSH (Machine.Val (Machine.B true)) ]
  | K.FALSE ->
      [ Machine.PUSH (Machine.Val (Machine.B false)) ]
  | K.UNIT ->
      [ Machine.PUSH (Machine.Val Machine.Unit) ]
  | K.VAR x ->
      [ Machine.PUSH (Machine.Id x); Machine.LOAD ]
  | K.EQUAL (e1, e2) ->
      trans e1 @ trans e2 @ [ Machine.EQ ]
  | K.LESS (e1, e2) ->
      trans e1 @ trans e2 @ [ Machine.LESS ]
  | K.NOT e ->
      trans e @ [ Machine.NOT ]
  | K.SEQ (e1, e2) ->
      trans e1 @ [ Machine.POP ] @ trans e2
  | K.ASSIGN (x, e) ->
      trans e @ [ Machine.PUSH (Machine.Id x); Machine.STORE ]
  | K.IF (e1, e2, e3) ->
      trans e1 @ [ Machine.JTR (trans e2, trans e3) ]
  | K.WHILE (cond, body) ->
      let loop =
        trans cond
        @ [ Machine.JTR (trans body @ [ Machine.POP ] @ trans (K.WHILE (cond, body)), [ Machine.PUSH (Machine.Val Machine.Unit) ]) ]
      in
      loop
  | K.FOR (x, e1, e2, body) ->
      let start = K.VAR "#start"
      and endv = K.VAR "#end"
      and var = K.VAR x in
      trans (K.LETV ("#start", e1,
              K.LETV ("#end", e2,
              K.LETV (x, start,
                K.WHILE (
                  K.LESS (var, K.ADD(endv, K.NUM 1)),
                  K.SEQ (body,
                    K.ASSIGN (x, K.ADD (var, K.NUM 1))
                  )
                )))))

  | K.LETF (f, x, e1, e2) ->
      [ Machine.PUSH (Machine.Fn (x, [Machine.BIND f] @ trans e1)); Machine.BIND f] @ trans e2 @ [ Machine.UNBIND; Machine.POP ]
  | K.CALLV (f, e) ->
    [ Machine.PUSH (Machine.Id f); Machine.LOAD ] @
    trans e @
    [ Machine.MALLOC;
      Machine.BIND "#tmp";
      Machine.PUSH (Machine.Id "#tmp"); Machine.STORE;
      Machine.PUSH (Machine.Id "#tmp");
      Machine.CALL ]


    | K.CALLR (f, x) ->
      [ Machine.PUSH (Machine.Id x); Machine.PUSH (Machine.Id f); Machine.LOAD; Machine.CALL ]
  | K.WRITE e ->
    trans e @ [ Machine.PUT ]


