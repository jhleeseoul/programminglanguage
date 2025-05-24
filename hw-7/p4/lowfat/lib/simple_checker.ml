open M

exception TypeError of string

type var = string
type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc   of typ
  | TFun   of typ * typ
  | TVar   of var

let counter = ref 0
let new_var () =
  incr counter;
  "α" ^ string_of_int !counter

type subst = (var * typ) list
let empty_subst : subst = []

let rec apply_subst (s:subst) (t:typ) : typ =
  match t with
  | TInt | TBool | TString -> t
  | TPair(a,b) -> TPair(apply_subst s a, apply_subst s b)
  | TLoc a     -> TLoc   (apply_subst s a)
  | TFun(a,b)  -> TFun   (apply_subst s a, apply_subst s b)
  | TVar v     ->
    (match List.assoc_opt v s with
     | Some t' -> apply_subst s t'
     | None    -> TVar v)

let rec occurs (x:var) (t:typ) : bool =
  match t with
  | TVar v           -> v = x
  | TPair(a,b)
  | TFun(a,b)        -> occurs x a || occurs x b
  | TLoc a           -> occurs x a
  | _                -> false

let rec unify (t1:typ) (t2:typ) (s:subst) : subst =
  let t1 = apply_subst s t1
  and t2 = apply_subst s t2 in
  match (t1, t2) with
  | TInt, TInt | TBool, TBool | TString, TString ->
      s
  | TVar v, t | t, TVar v ->
      if t = TVar v then s
      else if occurs v t then
        raise (TypeError ("Occurs-check failed on "^v))
      else
        (v, t) :: s
  | TPair(a1,a2), TPair(b1,b2)
  | TFun(a1,a2),  TFun(b1,b2) ->
      let s1 = unify a1 b1 s in
      unify (apply_subst s1 a2) (apply_subst s1 b2) s1
  | TLoc a, TLoc b ->
      unify a b s
  | _ ->
      raise (TypeError "Unification failed") 

let rec infer (env:(var * typ) list) (e:M.exp) (s:subst) : typ * subst =
  match e with
  | CONST (N _) ->
      (TInt, s)             
  | CONST (B _) ->
      (TBool, s)                 
  | CONST (S _) ->
      (TString, s)               
  | VAR x ->
      (match List.assoc_opt x env with
       | Some t -> (t, s)
       | None   -> raise (TypeError ("Unbound variable "^x)))
  | FN (x, body) ->
      let tv = TVar (new_var ()) in
      let env' = (x,tv)::env in
      let (t_body, s1) = infer env' body s in
      (TFun(apply_subst s1 tv, t_body), s1)    

  | APP (f, arg) ->
      let (t_f, s1) = infer env f s in
      let (t_a, s2) = infer env arg s1 in
      let tv = TVar (new_var ()) in
      let s3 = unify (apply_subst s2 t_f) (TFun(t_a, tv)) s2 in
      (apply_subst s3 tv, s3)            

  | LET (d, body) ->
      (match d with
       | VAL (x, e1) ->
           let (t1, s1) = infer env e1 s in
           infer ((x,t1)::env) body s1           
       | REC (f, x, e1) ->
           let tv_arg = TVar (new_var ()) in
           let tv_res = TVar (new_var ()) in
           let tfun  = TFun(tv_arg, tv_res) in
           let env'  = (f,tfun)::(x,tv_arg)::env in
           let (t1, s1) = infer env' e1 s in
           let s2 = unify (apply_subst s1 tv_res) t1 s1 in
           infer ((f, apply_subst s2 tfun)::env) body s2)

  | IF (c,e1,e2) ->
      let (tc, s1) = infer env c s in
      let s2 = unify tc TBool s1 in
      let (t1, s3) = infer env e1 s2 in
      let (t2, s4) = infer env e2 s3 in
      let s5 = unify (apply_subst s4 t1) (apply_subst s4 t2) s4 in
      (apply_subst s5 t1, s5)               

  | BOP (op, e1, e2) ->
      let (t1, s1) = infer env e1 s in
      let (t2, s2) = infer env e2 s1 in
      let s3 =
        (match op with
         | ADD | SUB ->
             let s' = unify t1 TInt s2 in unify t2 TInt s'
         | AND | OR  ->
             let s' = unify t1 TBool s2 in unify t2 TBool s'
         | EQ        ->
             unify t1 t2 s2) in
      let tout = (match op with EQ|AND|OR -> TBool | _ -> TInt) in
      (tout, s3)                        

  | READ ->
      (TInt, s)                   

  | WRITE e1 ->
      let (t1, s1) = infer env e1 s in
      (match apply_subst s1 t1 with
       | TInt | TBool | TString -> (t1, s1)
       | _ -> raise (TypeError "WRITE: unsupported type"))  

  | PAIR (e1, e2) ->
      let (t1, s1) = infer env e1 s in
      let (t2, s2) = infer env e2 s1 in
      (TPair(t1,t2), s2)          

  | FST e1 ->
      let (t1, s1) = infer env e1 s in
      let tv1 = TVar (new_var ()) in
      let tv2 = TVar (new_var ()) in
      let s2 = unify t1 (TPair(tv1,tv2)) s1 in
      (apply_subst s2 tv1, s2)      

  | SND e1 ->
      let (t1, s1) = infer env e1 s in
      let tv1 = TVar (new_var ()) in
      let tv2 = TVar (new_var ()) in
      let s2 = unify t1 (TPair(tv1,tv2)) s1 in
      (apply_subst s2 tv2, s2)     

  | MALLOC e1 ->
      let (t1, s1) = infer env e1 s in
      (TLoc t1, s1)          

  | ASSIGN (e1, e2) ->
      let (t1, s1) = infer env e1 s in
      let (t2, s2) = infer env e2 s1 in
      let tv = TVar (new_var ()) in
      let s3 = unify t1 (TLoc tv) s2 in
      let s4 = unify t2 tv s3 in
      (tv, s4)                    
  | BANG e1 ->
      let (t1, s1) = infer env e1 s in
      let tv = TVar (new_var ()) in
      let s2 = unify t1 (TLoc tv) s1 in
      (apply_subst s2 tv, s2) 

  | SEQ (e1, e2) ->
      let (_, s1) = infer env e1 s in
      infer env e2 s1                     

let check (exp : M.exp) : M.types =
  let (t_infer, subst) = infer [] exp empty_subst in
  let t_final = apply_subst subst t_infer in
  let rec to_mtype = function
    | TInt        -> M.TyInt
    | TBool       -> M.TyBool
    | TString     -> M.TyString
    | TPair(a,b)  -> M.TyPair(to_mtype a, to_mtype b)
    | TLoc a      -> M.TyLoc (to_mtype a)
    | TFun(a,b)   -> M.TyArrow(to_mtype a, to_mtype b) 
    | TVar v      ->
        raise (M.TypeError ("Unresolved type variable: "^v))
  in
  to_mtype t_final

