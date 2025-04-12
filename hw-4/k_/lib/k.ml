(*
 * SNU 4190.310 Programming Languages 2025 Spring
 *  K- Interpreter
 *)

(** Location Signature *)
module type LOC = sig
  type t

  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC = struct
  type t = Location of int

  let base = Location 0
  let equal (Location a) (Location b) = a = b
  let diff (Location a) (Location b) = a - b
  let increase (Location base) n = Location (base + n)
end

(** Memory Signature *)
module type MEM = sig
  type 'a t

  exception Not_allocated
  exception Not_initialized

  val empty : 'a t
  (** get empty memory *)

  val load : 'a t -> Loc.t -> 'a
  (** load value : Mem.load mem loc => value *)

  val store : 'a t -> Loc.t -> 'a -> 'a t
  (** save value : Mem.store mem loc value => mem' *)

  val alloc : 'a t -> Loc.t * 'a t
  (** get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(** Environment Signature *)
module type ENV = sig
  type ('a, 'b) t

  exception Not_bound

  val empty : ('a, 'b) t
  (** get empty environment *)

  val lookup : ('a, 'b) t -> 'a -> 'b
  (** lookup environment : Env.lookup env key => content *)

  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (** id binding : Env.bind env key content => env'*)
end

(** Memory Implementation *)
module Mem : MEM = struct
  exception Not_allocated
  exception Not_initialized

  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list

  let empty = M (Loc.base, [])

  let rec replace_nth l n c =
    match l with
    | h :: t -> if n = 1 then c :: t else h :: replace_nth t (n - 1) c
    | [] -> raise Not_allocated

  let load (M (boundary, storage)) loc =
    match List.nth storage (Loc.diff boundary loc - 1) with
    | V v -> v
    | U -> raise Not_initialized

  let store (M (boundary, storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary, storage)) =
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(** Environment Implementation *)
module Env : ENV = struct
  exception Not_bound

  type ('a, 'b) t = E of ('a -> 'b)

  let empty = E (fun x -> raise Not_bound)
  let lookup (E env) id = env id
  let bind (E env) id loc = E (fun x -> if x = id then loc else env x)
end

(** K- Interpreter *)
module type KMINUS = sig
  exception Error of string

  type id = string

  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp  (** sequence *)
    | IF of exp * exp * exp  (** if-then-else *)
    | WHILE of exp * exp  (** while loop *)
    | LETV of id * exp * exp  (** variable binding *)
    | LETF of id * id list * exp * exp  (** procedure binding *)
    | CALLV of id * exp list  (** call by value *)
    | CALLR of id * id list  (** call by referenece *)
    | RECORD of (id * exp) list  (** record construction *)
    | FIELD of exp * id  (** access record field *)
    | ASSIGN of id * exp  (** assgin to variable *)
    | ASSIGNF of exp * id * exp  (** assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type memory
  type env
  type value = Num of int | Bool of bool | Unit | Record of (id -> Loc.t)

  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS = struct
  exception Error of string

  type id = string

  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp  (** sequence *)
    | IF of exp * exp * exp  (** if-then-else *)
    | WHILE of exp * exp  (** while loop *)
    | LETV of id * exp * exp  (** variable binding *)
    | LETF of id * id list * exp * exp  (** procedure binding *)
    | CALLV of id * exp list  (** call by value *)
    | CALLR of id * id list  (** call by referenece *)
    | RECORD of (id * exp) list  (** record construction *)
    | FIELD of exp * id  (** access record field *)
    | ASSIGN of id * exp  (** assgin to variable *)
    | ASSIGNF of exp * id * exp  (** assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type value = Num of int | Bool of bool | Unit | Record of (id -> Loc.t)
  type memory = value Mem.t

  type env = (id, env_entry) Env.t
  and env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with Num n -> n | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with Bool b -> b | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with Unit -> () | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with Record r -> r | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc")
      | Proc (id_list, exp, env) -> (id_list, exp, env)
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
    | READ x ->
        let v = Num (read_int ()) in
        let l = lookup_env_loc env x in
        (v, Mem.store mem l v)
    | WRITE e ->
        let v, mem' = eval mem env e in
        let n = value_int v in
        let _ = print_endline (string_of_int n) in
        (v, mem')
    | LETV (x, e1, e2) ->
        let v, mem' = eval mem env e1 in
        let l, mem'' = Mem.alloc mem' in
        eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
        let v, mem' = eval mem env e in
        let l = lookup_env_loc env x in
        (v, Mem.store mem' l v)
    | NUM n -> (Num n, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | UNIT -> (Unit, mem)
    | VAR x ->
        let l = lookup_env_loc env x in
        let v = Mem.load mem l in
        (v, mem)
    | ADD (e1, e2) ->
        let v1, mem1 = eval mem env e1 in
        let v2, mem2 = eval mem1 env e2 in
        (Num (value_int v1 + value_int v2), mem2)
    | SUB (e1, e2) ->
        let v1, mem1 = eval mem env e1 in
        let v2, mem2 = eval mem1 env e2 in
        (Num (value_int v1 - value_int v2), mem2)
    | MUL (e1, e2) ->
        let v1, mem1 = eval mem env e1 in
        let v2, mem2 = eval mem1 env e2 in
        (Num (value_int v1 * value_int v2), mem2)
    | DIV (e1, e2) ->
        let v1, mem1 = eval mem env e1 in
        let v2, mem2 = eval mem1 env e2 in
        let n2 = value_int v2 in
        if n2 = 0 then raise (Error "Division by zero")
        else (Num (value_int v1 / n2), mem2)
    | EQUAL (e1, e2) ->
        let v1, mem1 = eval mem env e1 in
        let v2, mem2 = eval mem1 env e2 in
        (Bool (v1 = v2), mem2)
    | LESS (e1, e2) ->
        let v1, mem1 = eval mem env e1 in
        let v2, mem2 = eval mem1 env e2 in
        (Bool (value_int v1 < value_int v2), mem2)
    | NOT e ->
        let v, mem1 = eval mem env e in
        (Bool (not (value_bool v)), mem1)
    | SEQ (e1, e2) ->
        let _, mem1 = eval mem env e1 in
        eval mem1 env e2
    | IF (e, e1, e2) ->
        let v_cond, mem1 = eval mem env e in
        if value_bool v_cond then eval mem1 env e1 else eval mem1 env e2
    | WHILE (cond, body) ->
        let rec loop mem =
          let v_cond, mem1 = eval mem env cond in
          if value_bool v_cond then
            let _, mem2 = eval mem1 env body in
            loop mem2
          else (Unit, mem1)
        in
        loop mem
    | LETF (f, params, fbody, cont) ->
        (* 함수 정의를 환경에 바인딩하고 cont 실행 *)
        let env' = Env.bind env f (Proc (params, fbody, env)) in
        eval mem env' cont
    | CALLV (f, args) ->
        let params, fbody, fenv = lookup_env_proc env f in
        if List.length params <> List.length args then
          raise (Error "InvalidArg");

        (* args는 호출 시점 env에서 평가 *)
        let rec eval_args mem args =
          match args with
          | [] -> ([], mem)
          | a :: rest ->
              let v, mem1 = eval mem env a in
              let vs, mem2 = eval_args mem1 rest in
              (v :: vs, mem2)
        in
        let arg_values, mem1 = eval_args mem args in

        (* 함수 정의 시점 env 기반으로 파라미터 바인딩, 메모리에 값도 저장 *)
        let rec bind_params mem env params values =
          match (params, values) with
          | [], [] -> (mem, env)
          | p :: ps, v :: vs ->
              let l, mem1 = Mem.alloc mem in
              let mem2 = Mem.store mem1 l v in
              let env' = Env.bind env p (Addr l) in
              bind_params mem2 env' ps vs
          | _ -> raise (Error "Should not happen")
        in

        let mem2, env_args = bind_params mem1 fenv params arg_values in
        (* 3단계: f 자신도 env에 다시 바인딩해서 재귀 가능하게 *)
        let env_full = Env.bind env_args f (Proc (params, fbody, fenv)) in
        (* 4단계: 본문 실행 *)
        (* 주의: 메모리는 호출 시점에서 가져와야 한다 *)
        (* 주의: fbody는 정의 시점 env에서 평가해야 한다 *)
        eval mem2 env_full fbody
    | CALLR (f, args) ->
        let params, fbody, fenv = lookup_env_proc env f in
        if List.length params <> List.length args then
          raise (Error "InvalidArg");

        (* 주의: 주소는 호출 위치 env에서 가져와야 한다 *)
        let rec bind_refs callsite_env fenv params args =
          match (params, args) with
          | [], [] -> fenv
          | p :: ps, a :: as_ ->
              let l = lookup_env_loc callsite_env a in
              let fenv' = Env.bind fenv p (Addr l) in
              bind_refs callsite_env fenv' ps as_
          | _ -> raise (Error "Should not happen")
        in

        let env_args = bind_refs env fenv params args in
        let env_full = Env.bind env_args f (Proc (params, fbody, fenv)) in
        eval mem env_full fbody
    | RECORD fields ->
        let rec eval_fields mem fields =
          match fields with
          | [] -> ([], mem)
          | (id, e) :: rest ->
              let v, mem1 = eval mem env e in
              let l, mem2 = Mem.alloc mem1 in
              let mem3 = Mem.store mem2 l v in
              let rest_bindings, mem4 = eval_fields mem3 rest in
              ((id, l) :: rest_bindings, mem4)
        in
        let bindings, mem' = eval_fields mem fields in
        let lookup_field id =
          try List.assoc id bindings
          with Not_found -> raise (Error ("No such field: " ^ id))
        in
        (Record lookup_field, mem')
    | FIELD (e, id) ->
        let v, mem1 = eval mem env e in
        let r = value_record v in
        let l = r id in
        let v' = Mem.load mem1 l in
        (v', mem1)
    | ASSIGNF (e1, id, e2) ->
        let v_rec, mem1 = eval mem env e1 in
        let r = value_record v_rec in
        let l = r id in
        let v_rhs, mem2 = eval mem1 env e2 in
        let mem3 = Mem.store mem2 l v_rhs in
        (v_rhs, mem3)

  (* Other cases are omitted for brevity *)
  let run (mem, env, pgm) =
    let v, _ = eval mem env pgm in
    v
end
