(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)


type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
   match exp with
  | Var x -> SS.singleton x
  | Num _ -> SS.empty
  | Bool _ -> SS.empty
  | Unop (_, y) -> free_vars y
  | Binop (_, x, y) -> SS.union (free_vars x) (free_vars y)
  | Conditional (i, t, e) ->  SS.union (SS.union (free_vars i) (free_vars t))
     (free_vars e)
  | Fun (v, e) -> SS.remove v (free_vars e)
  | Let (v, e1, e2) -> SS.union (SS.remove v (free_vars e2)) (free_vars e1)
  | Letrec (v, e1, e2) -> SS.union (SS.remove v (free_vars e2)) (free_vars e1)
  | Raise -> SS.empty
  | Unassigned ->SS.empty
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname : unit ->  varid =
  
  let suffix = ref ~-1 in
  fun () ->
    suffix := !suffix + 1;
    "x" ^ string_of_int !suffix ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)


  ;;

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
   match exp with
  | Var x -> if x = var_name then repl else Var x 
      (* ACCOUNT FOR FREE VAR *)
  | Num x -> Num x
  | Bool x -> Bool x
  | Unop (x, y) -> Unop (x, subst var_name repl y)
  | Binop (b, x, y) -> Binop (b, subst var_name repl x, subst var_name repl y)
  | Conditional (i, t, e) -> 
      Conditional (subst var_name repl i, subst var_name repl t, 
                  subst var_name repl e)
  | Fun (v, e) ->
    if v = var_name then Fun (v, e)
    else if not (SS.mem v (free_vars repl)) 
      then Fun (v, subst var_name repl e)
    else let new_var = new_varname () in
      Fun (new_var, subst v (Var new_var) (subst var_name repl e))
     (* HOW TO DO BOTH SUBSTITUTIONS AT ONCE? *)
  | Let (v, e1, e2) -> 
      if v = var_name then Let (v, subst var_name repl e1, e2)
      else if not (SS.mem v (free_vars repl)) 
        then Let (v, subst var_name repl e1, subst var_name repl e2)
      else let new_var = new_varname () in
      Let (new_var, subst var_name repl e1, subst var_name repl (subst v (Var new_var) e2))
      (* Let (new_var, subst v repl e1, 
          subst v (Var new_var) (subst var_name repl e2)) *)
   (* Let (y, Q, R) varname = x repl = P e2 = R
      Let(z, subst x p Q, subst y z (subst x p R)
      Let (new_var, subst varname repl e1, subst v new_var (subst varname repl e2)))
    *)



  | Letrec (v, e1, e2) -> 
      if v = var_name then Letrec (v, subst var_name repl e1, e2)
     else if not (SS.mem v (free_vars repl)) 
        then Letrec (v, subst var_name repl e1, subst var_name repl e2)
       else let new_var = new_varname () in
      Letrec (new_var, subst var_name repl e1, 
          subst v (Var new_var) (subst var_name repl e2))
    (* HOW TO DO BOTH SUBSTITUTIONS AT ONCE? *)
  | Raise -> Raise
  | Unassigned -> Unassigned
  | App (e1, e2) -> App (subst var_name repl e1, subst var_name repl e2)
  (* failwith "subst not implemented" ;; *)
  
  ;;
     
(*......................................................................
  String representations of expressions
 *)
   

(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)

let rec exp_to_concrete_string (exp : expr) : string =

  let concrete_binop (x : expr) (y : expr) (bin : string) : string =
    (exp_to_concrete_string x) ^ bin ^ (exp_to_concrete_string y)
  in

  match exp with 
  | Var x -> x
  | Num x -> string_of_int x
  | Bool x -> string_of_bool x
  | Unop (Negate, y) -> "-" ^ (exp_to_concrete_string y) 
  (* "~" ^ "(" ^ (exp_to_concrete_string y) ^ ")" *)
  | Binop (b, x, y) -> 
      (match b with
      | Plus -> concrete_binop x y " + "
      | Minus -> concrete_binop x y " - "
      | Times -> concrete_binop x y " * "
      | Equals -> concrete_binop x y " = "
      | LessThan-> concrete_binop x y " < ")
  | Conditional (i, t, e) -> "if " ^ (exp_to_concrete_string i) ^ " then " ^ 
                             (exp_to_concrete_string t) ^ " else " ^ 
                             (exp_to_concrete_string e)
  | Fun (v, e) -> "fun " ^ v ^ " -> " ^ (exp_to_concrete_string e)
  | Let (v, e1, e2) -> "let " ^ v ^ " = " ^ (exp_to_concrete_string e1) ^ 
                       " in " ^ (exp_to_concrete_string e2)
  | Letrec (v, e1, e2) -> "let rec " ^ v ^ " = " ^ (exp_to_concrete_string e1) 
                          ^ " in " ^ (exp_to_concrete_string e2)
  | Raise ->  "Raise"  
  | Unassigned -> "Unassigned" 
  | App (e1, e2) ->
     (exp_to_concrete_string e1) ^ " (" ^ (exp_to_concrete_string e2) ^ ")" ;;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =  
  (* failwith "exp_to_abstract_string not implemented" ;; *)
  let abstract_one_st (t : string) (v : string) : string =
    t ^ " (" ^ v ^ ")"
  in 

  let abstract_two (t : string) (v1 : expr) (v2 : expr): string =
    t ^ " (" ^ (exp_to_abstract_string v1) ^ ", " ^ 
      (exp_to_abstract_string v2) ^ ")"
  in 

  let abstract_three_ex (t : string) (v1 : expr) (v2 : expr) (v3 : expr) 
                      : string =
    abstract_one_st t ((exp_to_abstract_string v1) ^ ", " ^ 
    (exp_to_abstract_string v2) ^ ", " ^ (exp_to_abstract_string v3))
  in 

  let abstract_three_st (t : string) (v1 : string) (v2 : expr) (v3 : expr) 
                      : string =
    abstract_one_st t (v1 ^ ", " ^ (exp_to_abstract_string v2)
     ^ ", " ^ (exp_to_abstract_string v3))
  in 

  let abstract_binop (x : expr) (y : expr) (bin : string) : string =
    "Binop" ^ " (" ^ bin ^ ", " ^ (exp_to_abstract_string x) ^ ", " ^ 
      (exp_to_abstract_string y) ^ ")"
  in

  match exp with
  | Var x -> abstract_one_st "Var" x 
  | Num x -> abstract_one_st "Num" (string_of_int x)
  | Bool x -> abstract_one_st "Bool" (string_of_bool x)
  | Unop (_, y) -> "Unop (Negate, " ^ exp_to_abstract_string y ^ ")"
  | Binop (b, x, y) -> 
    (match b with
      | Plus -> abstract_binop x y "Plus"
      | Minus -> abstract_binop x y "Minus"
      | Times -> abstract_binop x y "Times"
      | Equals -> abstract_binop x y "Equals"
      | LessThan-> abstract_binop x y "LessThan")
  | Conditional (i, t, e) -> abstract_three_ex "Conditional" i t e
  | Fun (v, e) -> abstract_one_st "Fun" 
      (v ^ ", " ^ (exp_to_abstract_string e))
  | Let (v, e1, e2) -> abstract_three_st "Let" v e1 e2
  | Letrec (v, e1, e2) -> abstract_three_st "Letrec" v e1 e2
  | Raise ->  "Raise"
  | Unassigned -> "Unassigned"
  | App (e1, e2) -> "App" ^ " (" ^ (exp_to_abstract_string e1) ^ ", " ^ 
      (exp_to_abstract_string e2) ^ ")"
  ;;
