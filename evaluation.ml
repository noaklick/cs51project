(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)

open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error in
   the interpreter *)
exception EvalError of string ;;
  
(* Exception for evaluator runtime, generated by an explicit `raise`
   construct in the object language *)
exception EvalException ;;

(*......................................................................
  Environments and values 
 *)

module type ENV = sig
    (* the type of environments *)
    type env
    (* the type of values stored in environments *)
    type value =
      | Val of expr
      | Closure of (expr * env)
   
    (* empty () -- Returns an empty environment *)
    val empty : unit -> env

    (* close expr env -- Returns a closure for `expr` and its `env` *)
    val close : expr -> env -> value

    (* lookup env varid -- Returns the value in the `env` for the
       `varid`, raising an `Eval_error` if not found *)
    val lookup : env -> varid -> value

    (* extend env varid loc -- Returns a new environment just like
       `env` except that it maps the variable `varid` to the `value`
       stored at `loc`. This allows later changing the value, an
       ability used in the evaluation of `letrec`. To make good on
       this, extending an environment needs to preserve the previous
       bindings in a physical, not just structural, way. *)
    val extend : env -> varid -> value ref -> env

    (* env_to_string env -- Returns a printable string representation
       of environment `env` *)
    val env_to_string : env -> string
                                 
    (* value_to_string ?printenvp value -- Returns a printable string
       representation of a value; the optional flag `printenvp`
       (default: `true`) determines whether to include the environment
       in the string representation when called on a closure *)
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : ENV =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    let empty () : env = []

    let close (exp : expr) (env : env) : value =
      Closure (exp, env)

    let lookup (env : env) (varname : varid) : value =
      !(List.assoc varname env)
     
    let extend (env : env) (varname : varid) (loc : value ref) : env =
      (varname, loc) :: List.remove_assoc varname env
   
    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val v -> exp_to_concrete_string v
      | Closure (v, e) -> 
          if printenvp then 
            exp_to_concrete_string v ^ " where [" ^ env_to_string e ^ "]"
          else exp_to_concrete_string v

    and env_to_string (env : env) : string =
      let rec list_to_string (e : env) : string =
        match e with
        | [] -> ""
        | (v, r) :: tl -> "{" ^ v ^ "->" ^ (value_to_string !r) ^ "}" ^ (list_to_string tl)
      in
      "[" ^ list_to_string env ^ "]"

  end
;;


(*......................................................................
  Evaluation functions

  Each of the evaluation functions below evaluates an expression `exp`
  in an environment `env` returning a result of type `value`. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a `value` and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type `expr -> Env.env -> Env.value` for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). 

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as `eval_e` below.  *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   
let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

(* The SUBSTITUTION MODEL evaluator -- to be completed *)

let binop_eval  (op : binop) (v1 : expr) (v2 : expr) : expr = 
    match op, v1, v2 with
    | Plus, Num x1, Num x2 -> Num (x1 + x2)
    | Plus, Float x1, Float x2 -> Float (x1 +. x2)
    | Plus, _ , _ -> raise (EvalError "tried to add incompatible types")
    | Minus, Num x1, Num x2 -> Num (x1 - x2)
    | Minus, Float x1, Float x2 -> Float (x1 -. x2)
    | Minus, _ , _ -> raise (EvalError "tried to subtract incompatible types")
    | Times, Num x1, Num x2 -> Num (x1 * x2)
    | Times, Float x1, Float x2 -> Float (x1 *. x2)
    | Times, _ , _ -> raise (EvalError "tried to multiple incompatible types")
    | Equals, Num x1, Num x2 -> Bool (x1 = x2)
    | Equals, Float x1, Float x2 -> Bool (x1 = x2)
    | Equals, Bool x1, Bool x2 -> Bool (x1 = x2)
    | Equals, _ , _ -> raise (EvalError "tried to compare incompatible types")
    | LessThan, Num x1, Num x2 -> Bool (x1 < x2)
    | LessThan, Bool x1, Bool x2 -> Bool (x1 < x2)
    | LessThan, Float x1, Float x2 -> Bool (x1 < x2)
    | LessThan, _ , _ -> raise (EvalError "tried to compare incompatible types")
    | GreaterThan, Num x1, Num x2 -> Bool (x1 > x2)
    | GreaterThan, Bool x1, Bool x2 -> Bool (x1 > x2)
    | GreaterThan, Float x1, Float x2 -> Bool (x1 > x2)
    | GreaterThan, _ , _ -> 
        raise (EvalError "tried to compare incompatible types")

let eval_s (exp : expr) (_env : Env.env) : Env.value =

  let unop_eval_s (op : unop) (v : expr) : expr = 
    match op, v with
    | Negate, Num x -> Num (~-x)
    | _, _ -> raise (EvalError "unop not an op")
  in

  let rec eval_s_help (v : expr) : expr =
    match v with
    | Var x -> raise (EvalError "tried to evaluate a variable")
    | Num x -> Num x
    | Float x -> Float x
    | Bool x -> Bool x
    | Unop (x, y) -> eval_s_help (unop_eval_s x (eval_s_help y))
    | Binop (b, x, y) -> 
        eval_s_help (binop_eval b (eval_s_help x) (eval_s_help y))
    | Conditional (i, t, e) -> 
        (match eval_s_help i with
        | Bool x -> if x then eval_s_help t else eval_s_help e
        | _ -> raise (EvalError "conditional isn't a bool"))
    | Fun (v, e) -> Fun (v, e)
    | Let (v, e1, e2) -> eval_s_help (subst v (eval_s_help e1) e2)
    | Letrec (x, d, b) -> 
        let vd = eval_s_help d in
        let close = subst x (Letrec (x, vd, Var (x))) vd in
        let bclose = subst x (eval_s_help close) b in
        eval_s_help bclose
    | Raise -> Raise
    | Unassigned -> raise (EvalError "tried to evaluate unassigned")
    | App (p, q) -> 
       (match eval_s_help p with
        | Fun (x, b) -> 
            let vq = eval_s_help q in
            eval_s_help (subst x vq b)
        | _ -> raise (EvalError "tried to apply a non function"))
  in

  Env.Val (eval_s_help exp) ;;
     
(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)

let extract_val (v : Env.value) : expr =
  match v with
  | Val x -> x
  | Closure (x, y) -> x ;;
   
let rec eval_d (exp : expr) (env : Env.env) : Env.value =

  let unop_eval_d (op : unop) (v : expr) : expr = 
    match op, v with
    | Negate, Num x -> Num (~-x)
    | _, _ -> raise (EvalError "unop not an op")
  in

  let rec eval_d_help (v : expr) (en : Env.env) : expr =
    match v with
    | Var x -> raise (EvalError"something went wrong")
    | Num x -> Num x
    | Float x -> Float x
    | Bool x -> Bool x
    | Unop (x, y) -> unop_eval_d x (extract_val (eval_d y env))
    | Binop (b, x, y) ->
        (binop_eval b (extract_val (eval_d x env)) (extract_val (eval_d y env)))
    | Conditional (i, t, e) -> 
        (match extract_val (eval_d i env) with
        | Bool x -> if x then extract_val (eval_d t env) 
                    else extract_val (eval_d e env)
        | _ -> raise (EvalError "bool not a conditional"))
    | Fun (v, e) -> Fun (v, e)
    | Let (x, d, b) -> 
        let vd = eval_d d env in
        let vb = extract_val (eval_d b (Env.extend env x (ref vd))) in
        vb
    | Letrec (x, d, b) ->
      let x_ref = ref (Env.Val Unassigned) in
      let env_ext = Env.extend env x x_ref in 
      let vd = eval_d d env_ext in
      x_ref := vd;
      extract_val (eval_d b env_ext)
    | Raise -> Raise
    | Unassigned -> raise (EvalError "tried to evaluate unassigned")
    | App (p, q) -> 
        (match extract_val (eval_d p env) with 
        | Fun (x, b) ->
          let vq = eval_d q env in 
          let env_ext = Env.extend env x (ref vq) in
          let vb = extract_val (eval_d b env_ext) in
          vb
        | _ -> raise (EvalError "app did not have a function"))
  in

  match exp with 
  | Var x -> (try (Env.lookup env x) 
              with Not_found -> raise (EvalError "variable unbound"))
  | _ -> Env.Val (eval_d_help exp env)
  ;;
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
   
let rec eval_l (exp : expr) (env : Env.env) : Env.value =

  let unop_eval_l (op : unop) (v : expr) : expr = 
    match op, v with
    | Negate, Num x -> Num (~-x)
    | _, _ -> raise (EvalError "unop not an op")
  in

  (* handle all exp -> exp cases *)
  let eval_l_help (v : expr) (en : Env.env ) : expr =
    match v with
    | Num x -> Num x
    | Float x -> Float x
    | Bool x -> Bool x
    | Unop (x, y) -> unop_eval_l x (extract_val (eval_l y env))
    | Binop (b, x, y) -> 
      (binop_eval b (extract_val (eval_l x env)) (extract_val (eval_l y env)))
    | Conditional (i, t, e) ->
         (match extract_val (eval_l i env) with
        | Bool x -> if x then extract_val (eval_l t env) 
                    else extract_val (eval_l e env)
        | _ -> raise (EvalError "bool not a conditional"))
    | Raise  -> Raise
    | Unassigned -> raise (EvalError "tried to evaluate unassigned")
    | _ -> raise (EvalError "something went wrong")
  in

  match exp with
  | Var x -> Env.lookup env x
  | Fun (x, p) -> Closure (Fun (x,p), env)
  | App (p, q) -> 
      (match eval_l p env with 
      | Closure (Fun (x, b), env_l) ->
          let vq = eval_l q env in 
          let env_ext = Env.extend env_l x (ref vq) in
          let vb = eval_l b env_ext in
          vb
      | _ -> 
          raise (EvalError "app did not have a function")) 
  | Let (x, d, b) -> 
      let vd = eval_l d env in
      let vb = eval_l b (Env.extend env x (ref vd)) in
      vb
  | Letrec (x, d, b) ->
    let x_ref = ref (Env.Val Unassigned) in
    let env_ext = Env.extend env x x_ref in 
    let vd = eval_l d env_ext in
    x_ref := vd;
    eval_l b env_ext
  | _ -> Env.Val (eval_l_help exp env)
    ;;
  

(* The EXTENDED evaluator -- if you want, you can provide your
   extension as a separate evaluator, or if it is type- and
   correctness-compatible with one of the above, you can incorporate
   your extensions within `eval_s`, `eval_d`, or `eval_l`. *)

let eval_e _ =
  failwith "eval_e not implemented" ;;
  
(* Connecting the evaluators to the external world. The REPL in
   `miniml.ml` uses a call to the single function `evaluate` defined
   here. Initially, `evaluate` is the trivial evaluator `eval_t`. But
   you can define it to use any of the other evaluators as you proceed
   to implement them. (We will directly unit test the four evaluators
   above, not the `evaluate` function, so it doesn't matter how it's
   set when you submit your solution.) *)
   
let evaluate = eval_t ;;
let evaluate_s = eval_s ;;
let evaluate_d = eval_d ;;
let evaluate_l = eval_l ;;
