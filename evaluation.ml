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
      failwith "close not implemented"

    let lookup (env : env) (varname : varid) : value =
      failwith "lookup not implemented"

    let extend (env : env) (varname : varid) (loc : value ref) : env =
      failwith "extend not implemented"

    let value_to_string ?(printenvp : bool = true) (v : value) : string =
      failwith "value_to_string not implemented"

    let env_to_string (env : env) : string =
      failwith "env_to_string not implemented"
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
   
let rec eval_s (exp : expr) (_env : Env.env) : Env.value =
  let binop_eval_s  (op : binop) (v1 : expr) (v2 : expr) : expr = 
    match op, v1, v2 with
    | Plus, Num x1, Num x2 -> Num (x1 + x2)
    | Minus, Num x1, Num x2 -> Num (x1 - x2)
    | Times, Num x1, Num x2 -> Num (x1 * x2)
    | Equals, Num x1, Num x2 -> Bool (x1 = x2)
    | Equals, Bool x1, Bool x2 -> Bool (x1 = x2)
    | LessThan, Num x1, Num x2 -> Bool (x1 < x2)
    | LessThan, Bool x1, Bool x2 -> Bool (x1 = x2)
    | _, _, _ -> Raise
  in

  let unop_eval_s (op : unop) (v : expr) : expr = 
    match op, v with
    | Negate, Num x -> Num (~-x)
    | _, _ -> Raise
  in

  let rec eval_s_help (v : expr) : expr =
    match exp with
    | Var x -> Var x
    | Num x -> Num x
    | Bool x -> Bool x
    | Unop (x, y) -> unop_eval_s x (eval_s_help y)
    | Binop (b, x, y) -> binop_eval_s b (eval_s_help x) (eval_s_help y)
    | Conditional (i, t, e) -> 
        Conditional (eval_s_help i, eval_s_help t, eval_s_help e)
    | Fun (v, e) -> Fun (v, e)
    | Let (v, e1, e2) -> subst v (eval_s_help e1) e2
    | Letrec (v, e1, e2) -> subst v (eval_s_help e1) e2
    | Raise ->  Raise
    | Unassigned -> Unassigned
    | App (e1, e2) -> 
       (match eval_s_help e1 with
        | Fun (x, b) -> subst x (eval_s_help e2) b
        | _ -> Raise)
  in
  Env.Val (eval_s_help exp) ;;
  (* failwith "eval_s not implemented" ;; *)
     
(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)
   
let eval_d (_exp : expr) (_env : Env.env) : Env.value =
  failwith "eval_d not implemented" ;;
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
   
let eval_l (_exp : expr) (_env : Env.env) : Env.value =
  failwith "eval_l not implemented" ;;

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
   
let evaluate = eval_s ;;
