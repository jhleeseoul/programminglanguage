(*
 * SNU 4190.310 Programming Languages – M Interpreter
 *)

module M = struct
  (* --- AST 정의 (변경 없음) --- *)

  type id    = string
  type const = S of string | N of int | B of bool
  type bop   = ADD | SUB | EQ | AND | OR

  type exp =
    | CONST  of const
    | VAR    of id
    | FN     of id * exp
    | APP    of exp * exp
    | LET    of decl * exp
    | IF     of exp * exp * exp
    | BOP    of bop * exp * exp
    | READ
    | WRITE  of exp
    | PAIR   of exp * exp
    | FST    of exp
    | SND    of exp
    | MALLOC of exp
    | ASSIGN of exp * exp
    | BANG   of exp
    | SEQ    of exp * exp

  and decl =
    | REC of id * id * exp
    | VAL of id * exp

  (* --- 타입 정의 (원본 그대로 유지) --- *)

  type types =
    | TyInt
    | TyBool
    | TyString
    | TyPair of types * types
    | TyLoc of types
    | TyArrow of types * types

  (* --- 값 · 환경 · 메모리 (변경 없음) --- *)

  type loc = int

  type value =
    | Int     of int
    | Bool    of bool
    | String  of string
    | Pair    of value * value
    | Loc     of loc
    | Closure of fexpr * env

  and fexpr = 
    | Fun   of id * exp
    | RecFun of id * id * exp

  and env    = id -> value
  type memory = int * (loc -> value)

  exception RunError  of string
  exception TypeError of string

  (* 환경·메모리 기본 연산자 (변경 없음) *)
  let ( @+ ) f (x, v) y = if y = x then v else f y
  let store (l, m) (loc, v) = (l, m @+ (loc, v))
  let load  (_, m) loc     = m loc
  let malloc (l, m)        = (l, (l + 1, m))

  (* --- op2fn: 동적 검사 제거, 직접 패턴 매칭 --- *)
  let op2fn = function
    | ADD -> fun (Int i1,   Int i2)   -> Int  (i1 + i2)
    | SUB -> fun (Int i1,   Int i2)   -> Int  (i1 - i2)
    | EQ  -> fun (v1,       v2)       -> Bool (v1 = v2)
    | AND -> fun (Bool b1,  Bool b2)  -> Bool (b1 && b2)
    | OR  -> fun (Bool b1,  Bool b2)  -> Bool (b1 || b2)

  (* --- 출력 (변경 없음) --- *)
  let rec printValue = function
    | Int n    -> print_endline (string_of_int n)
    | Bool b   -> print_endline (string_of_bool b)
    | String s -> print_endline s
    | _        -> raise (TypeError "WRITE: unsupported value")

  (* --- eval: TODO 자리만 패턴 매칭으로 교체 --- *)
  let rec eval env mem exp =
    match exp with
    | CONST (S s)    -> (String s, mem)
    | CONST (N n)    -> (Int    n, mem)
    | CONST (B b)    -> (Bool   b, mem)
    | VAR x          -> (env x, mem)
    | FN (x, e)      -> (Closure (Fun (x, e), env), mem)

    | APP (e1, e2)   ->
        let v1, m'  = eval env mem  e1 in
        let v2, m'' = eval env m'   e2 in
        (match v1 with
         | Closure (Fun (x, body), env') ->
             eval (env' @+ (x, v2)) m'' body
         | Closure (RecFun (f, x, body), env') ->
             let rec_cl = Closure (RecFun (f, x, body), env') in
             eval ((env' @+ (f, rec_cl)) @+ (x, v2)) m'' body
         | _ ->
             raise (TypeError "APP: not a function"))

    | LET (d, body)  ->
        (match d with
         | VAL  (x, e1) ->
             let v1, m' = eval env mem e1 in
             eval (env @+ (x, v1)) m' body
         | REC  (f, x, e1) ->
             let rec_cl = Closure (RecFun (f, x, e1), env) in
             eval (env @+ (f, rec_cl)) mem body)

    | IF (e1, e2, e3) ->
        let v1, m' = eval env mem e1 in
        let Bool b = v1 in
        eval env m' (if b then e2 else e3)

    | BOP (op, e1, e2) ->
        let v1, m'  = eval env mem  e1 in
        let v2, m'' = eval env m'   e2 in
        ((op2fn op) (v1, v2), m'')

    | READ           ->
        let n = try read_int () with _ -> raise (RunError "read error") in
        (Int n, mem)

    | WRITE e        ->
        let v, m' = eval env mem e in
        let ()   = printValue v in
        (v, m')

    | PAIR (e1, e2)  ->
        let v1, m'  = eval env mem e1 in
        let v2, m'' = eval env m'   e2 in
        (Pair (v1, v2), m'')

    | FST e          ->
        let v, m' = eval env mem e in
        let Pair (a, _) = v in
        (a, m')

    | SND e          ->
        let v, m' = eval env mem e in
        let Pair (_, b) = v in
        (b, m')

    | MALLOC e       ->
        let v, m'    = eval env mem e in
        let loc, m'' = malloc m' in
        let m'''     = store m'' (loc, v) in
        (Loc loc, m''')

    | ASSIGN (e1, e2) ->
        let v1, m'  = eval env mem  e1 in
        let v2, m'' = eval env m'   e2 in
        let Loc loc = v1 in
        let m'''    = store m'' (loc, v2) in
        (v2, m''')

    | BANG e        ->
        let v, m'   = eval env mem e in
        let Loc loc = v in
        (load m' loc, m')

    | SEQ (e1, e2)  ->
        let _, m' = eval env mem e1 in
        eval env m' e2
end

(* --- 타입 검사기 서명 (원본 그대로 유지) --- *)
module type M_TypeChecker = sig
  val check : M.exp -> M.types
end
