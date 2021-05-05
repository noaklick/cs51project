(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)
   
let rec eval_d (exp : expr) (env : Env.env) : Env.value =
  (* evaluate d unops *)
  let unop_eval_d (op : unop) (v : expr) : expr = 
    match op, v with
    | Negate, Num x -> Num (~-x)
    | _, _ -> raise (EvalError "unop not an op")
  in

  (* to preserve expr -> expr *)
  let eval_d_help (v : expr) (en : Env.env) : expr =
    match v with
    | Num x -> Num x
    | Float x -> Float x
    | Bool x -> Bool x
    | Unop (x, y) -> unop_eval_d x (extract_val (eval_d y en))
    | Binop (b, x, y) ->
        (binop_eval b (extract_val (eval_d x en)) (extract_val (eval_d y en)))
    | Conditional (i, t, e) -> 
        (match extract_val (eval_d i en) with
        | Bool x -> if x then extract_val (eval_d t en) 
                    else extract_val (eval_d e en)
        | _ -> raise (EvalError "bool not a conditional"))
    | Fun (v, e) -> Fun (v, e)
    | Let (x, d, b) | Letrec (x, d, b) ->
      (* for dynamic semantics letrec works from the let rule *)
        let vd = eval_d d en in
        let vb = extract_val (eval_d b (Env.extend en x (ref vd))) in
        vb
    | Raise -> Raise
    | Unassigned -> raise (EvalError "tried to evaluate unassigned")
    | App (p, q) -> 
        (match extract_val (eval_d p en) with 
        | Fun (x, b) ->
          let vq = eval_d q en in 
          let env_ext = Env.extend en x (ref vq) in
          let vb = extract_val (eval_d b env_ext) in
          vb
        | _ -> raise (EvalError "app did not have a function"))
    | Var _ -> raise (EvalError"something went wrong")
  in

  match exp with 
  | Var x -> (try (Env.lookup env x) 
              with Not_found -> raise (EvalError "variable unbound"))
  | _ -> Env.Val (eval_d_help exp env)
  ;;
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
   
let rec eval_l (exp : expr) (env : Env.env) : Env.value =
  (* evaluate l unops *)
  let unop_eval_l (op : unop) (v : expr) : expr = 
    match op, v with
    | Negate, Num x -> Num (~-x)
    | _, _ -> raise (EvalError "unop not an op")
  in

  (* handle all exp -> exp cases *)
  let eval_l_help (v : expr) (en : Env.env) : expr =
    match v with
    | Num x -> Num x
    | Float x -> Float x
    | Bool x -> Bool x
    | Unop (x, y) -> unop_eval_l x (extract_val (eval_l y en))
    | Binop (b, x, y) -> 
      (binop_eval b (extract_val (eval_l x en)) (extract_val (eval_l y en)))
    | Conditional (i, t, e) ->
         (match extract_val (eval_l i en) with
        | Bool x -> if x then extract_val (eval_l t en) 
                    else extract_val (eval_l e en)
        | _ -> raise (EvalError "bool not a conditional"))
    | Raise  -> Raise
    | Unassigned -> raise (EvalError "tried to evaluate unassigned")
    | _ -> raise (EvalError "something went wrong")
  in

  (* handle all exp -> Env.value cases *)
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