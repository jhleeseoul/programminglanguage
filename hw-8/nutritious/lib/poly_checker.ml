(* Without LSP *)
(*
 * SNU 4190.310 Programming Languages
 * Type Checker (polymorphic let) with Algorithm W, value restriction, and eq restriction
 *)

open M

(* Type variables *)
type var = string

(* Internal type representation *)
type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var

(* Type schemes: simple or generalized *)
type typ_scheme =
  | SimpleTyp of typ
  | GenTyp of var list * typ

(* Type environment *)
type typ_env = (M.id * typ_scheme) list

(* Substitution: mapping type variable to type *)
type subst = (var * typ) list
let empty_subst : subst = []

let new_var_counter = ref 0
let new_var () =
  incr new_var_counter;
  "'a" ^ string_of_int !new_var_counter

(* Determine if an expression is a syntactic value (non-expansive) *)
let rec is_value = function
  | M.CONST _ -> true
  | FN _    -> true
  | _       -> false

(* Free type variables in a type *)
let rec ftv_of_typ = function
  | TInt | TBool | TString -> []
  | TVar a -> [a]
  | TPair (t1,t2) | TFun (t1,t2) -> ftv_of_typ t1 @ ftv_of_typ t2
  | TLoc t -> ftv_of_typ t

(* Free type variables in a scheme *)
let ftv_of_scheme = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp(vars, t) -> List.filter (fun a -> not (List.mem a vars)) (ftv_of_typ t)

(* Free type variables in an environment *)
let ftv_of_env env =
  List.flatten (List.map (fun (_, sch) -> ftv_of_scheme sch) env)

(* Substitution application to types *)
let rec subst_typ subst = function
  | TVar a -> (try List.assoc a subst with Not_found -> TVar a)
  | TPair(t1,t2) -> TPair(subst_typ subst t1, subst_typ subst t2)
  | TFun(t1,t2) -> TFun(subst_typ subst t1, subst_typ subst t2)
  | TLoc t -> TLoc (subst_typ subst t)
  | base -> base

(* Substitution application to schemes *)
let subst_scheme subst = function
  | SimpleTyp t -> SimpleTyp (subst_typ subst t)
  | GenTyp(vars, t) ->
    let subst' = List.filter (fun (a,_) -> not (List.mem a vars)) subst in
    GenTyp(vars, subst_typ subst' t)

(* Substitution application to environment *)
let subst_env subst env =
  List.map (fun (x, sch) -> (x, subst_scheme subst sch)) env

(* Composition of substitutions: s1 after s2 *)
let compose_subst s1 s2 =
  let s2' = List.map (fun (a,t) -> (a, subst_typ s1 t)) s2 in
  s1 @ s2'

let make_subst a t = [(a,t)]

(* Unification algorithm *)
let rec unify t1 t2 =
  match (t1, t2) with
  | (TInt, TInt) | (TBool, TBool) | (TString, TString) -> empty_subst
  | (TVar a, t) | (t, TVar a) ->
    if t = TVar a then empty_subst
    else if List.mem a (ftv_of_typ t) then raise (M.TypeError("occur check:"^a))
    else make_subst a t
  | (TPair(a1,a2), TPair(b1,b2)) | (TFun(a1,a2), TFun(b1,b2)) ->
    let s1 = unify a1 b1 in
    let s2 = unify (subst_typ s1 a2) (subst_typ s1 b2) in
    compose_subst s2 s1
  | (TLoc u1, TLoc u2) -> unify u1 u2
  | _ -> raise (M.TypeError("unification failure"))

(* Instantiate a type scheme: replace bound variables with fresh type variables *)
let instantiate = function
  | SimpleTyp t -> t
  | GenTyp(vars, t) ->
    let subs = List.map (fun a -> (a, TVar (new_var ()))) vars in
    subst_typ subs t

(* Generalize a type wrt. an environment *)
let generalize env t =
  let env_ftv = ftv_of_env env in
  let t_ftv = ftv_of_typ t in
  let vars = List.filter (fun a -> not (List.mem a env_ftv)) t_ftv in
  if vars = [] then SimpleTyp t else GenTyp(vars, t)

(* Top-level type checker using Algorithm W with value restriction and eq restriction *)
let check (exp : M.exp) : M.typ =
  let rec infer env e =
    match e with
    | M.CONST (N _) -> (empty_subst, TInt)
    | M.CONST (B _) -> (empty_subst, TBool)
    | M.CONST (S _) -> (empty_subst, TString)

    | M.VAR x ->
      (match List.assoc_opt x env with
       | Some sch -> (empty_subst, instantiate sch)
       | None -> raise (M.TypeError("unbound var: "^x)))

    | FN (x, body) ->
      let a = new_var () in
      let env1 = (x, SimpleTyp (TVar a)) :: env in
      let (s1, t1) = infer env1 body in
      (s1, TFun(subst_typ s1 (TVar a), t1))

    | APP (f, arg) ->
      let (s1, t_f) = infer env f in
      let env1 = subst_env s1 env in
      let (s2, t_arg) = infer env1 arg in
      let a = new_var () in
      let s3 = unify (subst_typ s2 t_f) (TFun(t_arg, TVar a)) in
      (compose_subst s3 (compose_subst s2 s1), subst_typ s3 (TVar a))

    | LET (d, body) ->
      (match d with
       | VAL (x, rhs) ->
         let (s1, t1) = infer env rhs in
         let env1 = subst_env s1 env in
         let sch = if is_value rhs then generalize env1 t1 else SimpleTyp t1 in
         let env2 = (x, sch) :: env1 in
         let (s2, t2) = infer env2 body in
         (compose_subst s2 s1, t2)

       | REC (f, x, fbody) ->
         let a = new_var () and b = new_var () in
         let t_fun = TFun(TVar a, TVar b) in
         let env1 = (f, SimpleTyp t_fun) :: env in
         let env2 = (x, SimpleTyp (TVar a)) :: env1 in
         let (s1, tf) = infer env2 fbody in
         let tf' = subst_typ s1 tf in
         let s2 = unify tf' t_fun in
         let t_fun' = subst_typ (compose_subst s2 s1) t_fun in
         let sch = generalize (subst_env (compose_subst s2 s1) env) t_fun' in
         let env3 = (f, sch) :: env in
         let env3' = subst_env (compose_subst s2 s1) env3 in
         let (s3, t3) = infer env3' body in
         (compose_subst s3 (compose_subst s2 s1), t3))

    | IF (cond, t1, t2) ->
      let (s1, tc) = infer env cond in
      let s_bool = unify tc TBool in
      let env1 = subst_env s_bool (subst_env s1 env) in
      let (s2, t_then) = infer env1 t1 in
      let env2 = subst_env s2 env1 in
      let (s3, t_else) = infer env2 t2 in
      let s4 = unify (subst_typ s3 t_then) t_else in
      (compose_subst s4 (compose_subst s3 (compose_subst s2 (compose_subst s_bool s1))), subst_typ s4 t_else)

    | BOP (op, e1, e2) ->
      let (s1, t1) = infer env e1 in
      let env1 = subst_env s1 env in
      let (s2, t2) = infer env1 e2 in
      let s12 = compose_subst s2 s1 in
      (match op with
       | ADD | SUB ->
         let s3 = unify (subst_typ s2 (subst_typ s1 t1)) TInt in
         let s4 = unify (subst_typ s3 (subst_typ s2 (subst_typ s1 t2))) TInt in
         (compose_subst s4 (compose_subst s3 s12), TInt)
       | AND | OR ->
         let s3 = unify (subst_typ s2 (subst_typ s1 t1)) TBool in
         let s4 = unify (subst_typ s3 (subst_typ s2 (subst_typ s1 t2))) TBool in
         (compose_subst s4 (compose_subst s3 s12), TBool)
       | EQ ->
         let s3 = unify (subst_typ s2 (subst_typ s1 t1)) t2 in
         let s123 = compose_subst s3 s12 in
         let t_eq = subst_typ s123 t1 in
         (match t_eq with
          | TInt | TBool | TString -> (s123, TBool)
          | _ -> raise (M.TypeError("eq requires int, bool, or string"))))

    | READ -> (empty_subst, TInt)

    | WRITE e -> infer env e

    | MALLOC e ->
      let (s, t) = infer env e in
      (s, TLoc t)

    | ASSIGN (l, r) ->
      let (s1, tl) = infer env l in
      let (s2, tr) = infer (subst_env s1 env) r in
      let s3 = unify (subst_typ s2 tl) (TLoc tr) in
      (compose_subst s3 (compose_subst s2 s1), tr)

    | BANG e ->
      let (s, t) = infer env e in
      (match t with
       | TLoc u -> (s, u)
       | _ -> raise (M.TypeError("bang requires loc")))

    | SEQ (e1, e2) ->
      let (s1, _) = infer env e1 in
      let (s2, t2) = infer (subst_env s1 env) e2 in
      (compose_subst s2 s1, t2)

    | PAIR (l, r) ->
      let (s1, tl) = infer env l in
      let (s2, tr) = infer (subst_env s1 env) r in
      (compose_subst s2 s1, TPair (tl, tr))

    | FST e ->
      let (s, t) = infer env e in
      let a = new_var () and b = new_var () in
      let s' = unify t (TPair (TVar a, TVar b)) in
      (s', subst_typ s' (TVar a))

    | SND e ->
      let (s, t) = infer env e in
      let a = new_var () and b = new_var () in
      let s' = unify t (TPair (TVar a, TVar b)) in
      (s', subst_typ s' (TVar b))
  in
  let (_, t_res) = infer [] exp in
  let rec to_mtyp = function
    | TInt -> M.TyInt
    | TBool -> M.TyBool
    | TString -> M.TyString
    | TPair (a, b) -> M.TyPair (to_mtyp a, to_mtyp b)
    | TLoc u -> M.TyLoc (to_mtyp u)
    | _ -> raise (M.TypeError("unexpected top-level"))
  in
  to_mtyp t_res
