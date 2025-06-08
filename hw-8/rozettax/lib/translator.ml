(*
 * SNU 4190.310 Programming Languages 
 * Homework "RozettaX" Complete Translator
 *)

(* translate SM5 values to Sonata values *)
let trans_v : Sm5.value -> Sonata.value = function
  | Sm5.Z z    -> Sonata.Z z
  | Sm5.B b    -> Sonata.B b
  | Sm5.L _    -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit   -> Sonata.Unit
  | Sm5.R _    -> raise (Sonata.Error "Invalid input program : pushing record")

(* mutual recursion for translating commands and objects *)
let rec trans : Sm5.command -> Sonata.command = fun command ->
  trans' command

and trans_obj : Sm5.obj -> Sonata.obj = function
  | Sm5.Val v         -> Sonata.Val (trans_v v)
  | Sm5.Id id         -> Sonata.Id id
  | Sm5.Fn (x, comm)  -> Sonata.Fn (x, trans comm)

and trans' : Sm5.command -> Sonata.command = function
  | Sm5.PUSH obj :: cmds  -> Sonata.PUSH (trans_obj obj) :: trans' cmds
  | Sm5.POP :: cmds       -> Sonata.POP               :: trans' cmds
  | Sm5.STORE :: cmds     -> Sonata.STORE             :: trans' cmds
  | Sm5.LOAD :: cmds      -> Sonata.LOAD              :: trans' cmds
  | Sm5.JTR (c1, c2) :: cmds
    -> Sonata.JTR (trans c1, trans c2)        :: trans' cmds
  | Sm5.MALLOC :: cmds    -> Sonata.MALLOC           :: trans' cmds
  | Sm5.BOX z :: cmds     -> Sonata.BOX z           :: trans' cmds
  | Sm5.UNBOX x :: cmds   -> Sonata.UNBOX x         :: trans' cmds
  | Sm5.BIND x :: cmds    -> Sonata.BIND x          :: trans' cmds
  | Sm5.UNBIND :: cmds    -> Sonata.UNBIND          :: trans' cmds
  | Sm5.GET :: cmds       -> Sonata.GET              :: trans' cmds
  | Sm5.PUT :: cmds       -> Sonata.PUT              :: trans' cmds
  | Sm5.CALL :: cmds      -> Sonata.CALL             :: trans' cmds
  | Sm5.ADD :: cmds       -> Sonata.ADD              :: trans' cmds
  | Sm5.SUB :: cmds       -> Sonata.SUB              :: trans' cmds
  | Sm5.MUL :: cmds       -> Sonata.MUL              :: trans' cmds
  | Sm5.DIV :: cmds       -> Sonata.DIV              :: trans' cmds
  | Sm5.EQ :: cmds        -> Sonata.EQ               :: trans' cmds
  | Sm5.LESS :: cmds      -> Sonata.LESS             :: trans' cmds
  | Sm5.NOT :: cmds       -> Sonata.NOT              :: trans' cmds
  | []                    -> []
