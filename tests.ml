(* 
                          CS 51 Final Project
                                MiniML
                               Testing
 *)
open Miniml;;
open Expr;;
open Evaluation;;

open CS51Utils ;; 
open Absbook ;;

let subst_test () = 
    (* (x + 1)[x ↦ 50]  *)
    let test1 = str_to_exp "x+1;;" in
    let test1st = subst "x" (Num (50)) test1 in
    print_string "substitution basic";
    print_newline ();
    print_string (exp_to_abstract_string test1st) ;
    print_newline ();
    print_string (exp_to_concrete_string test1st);
    print_newline ();
    unit_test (subst "x" (Num (50)) test1 = Binop (Plus,Num (50),Num (1)))
            "subst basic";
    print_newline ();

    (* (let x = y * y in x + x)[x ↦ 3] *)
    let test2 = str_to_exp "let x = y * y in x + x;;" in
    let test2st = subst "x" (Num(3)) test2 in
    print_string "substitution basic2";
    print_newline ();
    print_string (exp_to_abstract_string test2st) ;
    print_newline ();
    print_string (exp_to_concrete_string test2st);
    print_newline ();
    unit_test (subst "x" (Num(3)) test2 = str_to_exp ("let x = y*y in x + x;;"))
            "subst basic2";
    print_newline ();

    (* (let x = y * y in x + x)[y ↦ 3] *)
    let test3 = str_to_exp "(let x = y * y in x + x);;" in
    let test3st = subst "y" (Num(3)) test3 in
    print_string "substitution basic3";
    print_newline ();
    print_string (exp_to_abstract_string test3st) ;
    print_newline ();
    print_string (exp_to_concrete_string test3st);
    print_newline ();
    unit_test (subst "y" (Num(3)) test3 = str_to_exp "let x = 3 * 3 in x + x;;")
            "subst basic3";
    print_newline ();

    (*  ((fun x -> x * x) (x - 2)) [x ↦ 8] *)
    let test4 = str_to_exp " ((fun x -> x * x) (x - 2)) ;;" in
    unit_test (subst "x" (Num(8)) test4 = str_to_exp "(fun x -> x * x) (8 - 2);;")
            "subst basic4";

    (*  3 [x ↦ 8] *)
    let test5 = str_to_exp "3;;" in
    unit_test (subst "x" (Num(8)) test5 = str_to_exp "3;;")
            "subst basic5";
    
    (*  y [x ↦ 8] *)
    let test6 = str_to_exp "y;;" in
    unit_test (subst "x" (Num(8)) test6 = str_to_exp "y;;")
            "subst basic6";
    
    (*  x + y [x ↦ 8] *)
    let test6 = str_to_exp "x+y;;" in
    unit_test (subst "x" (Num(8)) test6 = str_to_exp "8+y;;")
            "subst basic6";
    
    (* (let x = 5 in f y) [y -> x + 1] *)
    let test7 = str_to_exp "let x = 5 in f y;;" in
    let test7st = subst "y" (str_to_exp "x+y;;") test7 in
    print_string "substitution basic7";
    print_newline ();
    print_string (exp_to_abstract_string test7st) ;
    print_newline ();
    print_string (exp_to_concrete_string test7st);
    print_newline ();
    unit_test (subst "y" (str_to_exp "x+y;;") test7 = str_to_exp "let x0 = 5 in f (z + 1);;")
            "subst basic7";

;;
let extract_val (v : Evaluation.Env.value) : expr =
    match v with
    | Val x -> x
    | Closure (x, y) -> x


let eval_s_test () =
    (* ~-(3+5)  *)
    let test1 = str_to_exp "~-(3+5);;" in
    print_string (exp_to_abstract_string test1) ;
    print_string (exp_to_concrete_string test1) ;
    let test1st = eval_s test1 (Env.empty ()) in
    let test1ex = extract_val test1st in
    print_string "eval_s basic";
    print_newline ();
    print_string (exp_to_abstract_string test1ex) ;
    print_newline ();
    print_string (exp_to_concrete_string test1ex);
    print_newline ();
    (* unit_test (subst "x" (Num (50)) test1 = Binop (Plus,Num (50),Num (1)))
            "subst basic"; *)
    print_newline ();


;;


let test_all () = 
    subst_test ();
    eval_s_test () ;;

let _ = test_all () ;;