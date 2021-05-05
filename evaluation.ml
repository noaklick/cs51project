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

(* evaluate all binops *)  (* floats added for extension *)
let binop_eval  (op : binop) (v1 : expr) (v2 : expr) : expr = 
    match op, v1, v2 with
    (* plus *)
    | Plus, Num x1, Num x2 -> Num (x1 + x2)
    | Plus, Float x1, Float x2 -> Float (x1 +. x2)
    | Plus, _ , _ -> raise (EvalError "tried to add incompatible types")

    (* minus *)
    | Minus, Num x1, Num x2 -> Num (x1 - x2)
    | Minus, Float x1, Float x2 -> Float (x1 -. x2)
    | Minus, _ , _ -> raise (EvalError "tried to subtract incompatible types")

    (* times *)
    | Times, Num x1, Num x2 -> Num (x1 * x2)
    | Times, Float x1, Float x2 -> Float (x1 *. x2)
    | Times, _ , _ -> raise (EvalError "tried to multiply incompatible types")

    (* divide *)
    | Divide, Num _, Num 0 -> raise (EvalError "divide by zero")
    | Divide, Num x1, Num x2 -> Num (x1 / x2)
    | Divide, Float _, Float 0. -> raise (EvalError "divide by zero")
    | Divide, Float x1, Float x2 -> Float (x1 /. x2)
    | Divide, _ , _ -> raise (EvalError "tried to divide incompatible types")

    (* equals *)
    | Equals, Num x1, Num x2 -> Bool (x1 = x2)
    | Equals, Float x1, Float x2 -> Bool (x1 = x2)
    | Equals, Bool x1, Bool x2 -> Bool (x1 = x2)

    (* less than *)
    | LessThan, Num x1, Num x2 -> Bool (x1 < x2)
    | LessThan, Bool x1, Bool x2 -> Bool (x1 < x2)
    | LessThan, Float x1, Float x2 -> Bool (x1 < x2)

    (* greater than *)
    | GreaterThan, Num x1, Num x2 -> Bool (x1 > x2)
    | GreaterThan, Bool x1, Bool x2 -> Bool (x1 > x2)
    | GreaterThan, Float x1, Float x2 -> Bool (x1 > x2)
    | _, _ , _ -> raise (EvalError "tried to compare incompatible types")

let eval_s (exp : expr) (_env : Env.env) : Env.value =

  (* evaluate s unops *)
  let unop_eval_s (op : unop) (v : expr) : expr = 
    match op, v with
    | Negate, Num x -> Num (~-x)
    | Negate, Float x -> Float (~-.x)
    | _, _ -> raise (EvalError "unop not an op")
  in

  (* to preserve expr -> expr *)
  let rec eval_s_help (v : expr) : expr =
    match v with
    | Var _ -> raise (EvalError "tried to evaluate a variable")
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
    | Let (x, d, b) -> eval_s_help (subst x (eval_s_help d) b)
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

(* function to get an expr from an Env.value *)
let extract_val (v : Env.value) : expr =
  match v with
  | Val x 
  | Closure (x, _) -> x ;;
  
(* for use in functor *)
type model = Dynamic | Lexical

module type MODEL =
sig
  val model : model
end

module LexEval : MODEL =
struct
   let model = Lexical
end

module DynEval : MODEL =
struct
  let model = Dynamic
end

module type EVALTYPE = 
  sig 
    val eval :  expr -> Env.env -> Env.value
  end

(* functor to create an eval function given a model of lexical or dynamic *)
module EvalLexDyn (Model : MODEL) : EVALTYPE  = 
struct
  (* unop evaluation is the same in both *)
  let unop_eval (op : unop) (v : expr) : expr = 
  match op, v with
  | Negate, Num x -> Num (~-x)
  | Negate, Float x ->Float (~-.x)
  | _, _ -> raise (EvalError "unop not an op")

  (* contains the evaluations that are the same in both *)
  let rec eval_same (exp : expr) (env : Env.env) : Env.value = 
    match exp with
    | Num x -> Env.Val (Num x)
    | Float x -> Env.Val (Float x)
    | Bool x -> Env.Val (Bool x)
    | Unop (x, y) -> Env.Val (unop_eval x (extract_val (eval y env)))
    | Binop (b, x, y) ->
        Env.Val (binop_eval b (extract_val (eval x env)) 
                              (extract_val (eval y env)))
    | Conditional (i, t, e) -> 
        (match extract_val (eval i env) with
        | Bool x -> if x then eval t env else eval e env
        | _ -> raise (EvalError "bool not a conditional"))
    | Raise -> Env.Val Raise
    | Unassigned -> raise (EvalError "tried to evaluate unassigned")
    | _ -> raise (EvalError "something went wrong")

    (* dynamic evaluation *)
    and eval_diff_dyn (exp : expr) (env : Env.env) : Env.value =
      match exp with 
      | Var x -> (try Env.lookup env x
                 with Not_found -> raise (EvalError "variable unbound"))
      | Fun (v, e) -> Env.Val (Fun (v, e))
      | Let (x, d, b) -> 
          let vd = eval d env in
          let vb = eval b (Env.extend env x (ref vd)) in
          vb
      | Letrec (x, d, b) ->
          let x_ref = ref (Env.Val Unassigned) in
          let env_ext = Env.extend env x x_ref in 
          let vd = eval d env_ext in
          x_ref := vd;
          eval b env_ext
      | App (p, q) -> 
          (match extract_val (eval p env) with 
          | Fun (x, b) ->
            let vq = eval q env in 
            let env_ext = Env.extend env x (ref vq) in
            let vb = eval b env_ext in
            vb
          | _ -> raise (EvalError "app did not have a function"))
        | _ -> raise (EvalError "something went wrong")

    (* lexical evaluation *)
    and eval_diff_lex (exp : expr) (env : Env.env) : Env.value =
      match exp with
      | Var x -> Env.lookup env x
      | Fun (x, p) -> Closure (Fun (x,p), env)
      | App (p, q) -> 
            (match eval p env with 
            | Closure (Fun (x, b), env_l) ->
                let vq = eval q env in 
                let env_ext = Env.extend env_l x (ref vq) in
                let vb = (eval b env_ext) in
                vb
            | _ -> raise (EvalError "app did not have a function")) 
      | Let (x, d, b) -> 
          let vd = eval d env in
          let vb = eval b (Env.extend env x (ref vd)) in
          vb
      | Letrec (x, d, b) ->
          let x_ref = ref (Env.Val Unassigned) in
          let env_ext = Env.extend env x x_ref in 
          let vd = eval d env_ext in
          x_ref := vd;
        eval b env_ext
      | _  -> raise (EvalError "something went wrong")

    and eval (exp : expr) (env : Env.env) : Env.value =
      match exp with 
      (* for evaluations that are the same *)
      | Num _ | Float _ | Bool _ | Unop _ | Binop _ | Conditional _ 
      | Raise | Unassigned -> eval_same exp env
      | _ -> 
        match Model.model with
        (* for evaluations which depend on model *)
        | Dynamic -> eval_diff_dyn exp env
        | Lexical -> eval_diff_lex exp env
end

(* modules for lexical and dynamic evaluation *)
module EvalLex = (EvalLexDyn (LexEval) : EVALTYPE)
module EvalDyn = (EvalLexDyn (DynEval) : EVALTYPE)
    
(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)
  let eval_d = EvalDyn.eval
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
  let eval_l = EvalLex.eval

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
