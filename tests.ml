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
    unit_test (subst "x" (Num (50)) test1 = Binop (Plus,Num (50),Num (1)))
            "subst basic";

    (* (let x = y * y in x + x)[x ↦ 3] *)
    let test2 = str_to_exp "let x = y * y in x + x;;" in
    unit_test (subst "x" (Num(3)) test2 = str_to_exp ("let x = y*y in x + x;;"))
            "subst basic2";

    (* (let x = y * y in x + x)[y ↦ 3] *)
    let test3 = str_to_exp "(let x = y * y in x + x);;" in
    unit_test (subst "y" (Num(3)) test3 = str_to_exp "let x = 3 * 3 in x + x;;")
            "subst basic3";
   

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
    let test7 = str_to_exp "x+y;;" in
    unit_test (subst "x" (Num(8)) test7 = str_to_exp "8+y;;")
            "subst basic7";
    
    (* (let x = 5 in f y) [y -> x + 1] *)
    let test8 = str_to_exp "let x = 5 in f y;;" in
    let test8st = subst "y" (str_to_exp "x+1;;") test8 in
    print_string "                                            substitution basic8";
    print_newline ();
    print_string"                                            let x0 = 5 in f (x + 1)";
    print_newline ();
    print_string ("                                            " ^ (exp_to_concrete_string test8st));
    print_newline ();
    unit_test (exp_to_concrete_string (subst "y" (str_to_exp "x+y;;") test8) = "let x0 = 5 in f (x + 1)")
            "subst basic8 dont freak out if fails";
;;

let extract_val (v : Evaluation.Env.value) : expr =
    match v with
    | Val x -> x
    | Closure (x, _) -> x ;;

let eval_s_test () =
    let eval_s_help_test (x : expr) = 
        eval_s x (Env.empty())
    in

    (* ~-(3+5)  *)
    let test1 = str_to_exp "~-(3+5);;" in
    let test1st = eval_s test1 (Env.empty ()) in
    let test1ex = extract_val test1st in
    unit_test (test1ex = str_to_exp "~-8;;")
            "eval_s basic1 dont freak out if fails";


     (* let x = 3 in x + 5 *)
    let test2 = str_to_exp "let x = 3 in x + 5;;" in
    let test2st = eval_s test2 (Env.empty ()) in
    let test2ex = extract_val test2st in
    unit_test (test2ex = str_to_exp "8;;")
            "eval_s basic2";

    (* 8-2*)
    let test3 = str_to_exp "8-2;;" in
    let test3st = eval_s test3 (Env.empty ()) in
    let test3ex = extract_val test3st in
    unit_test (test3ex = str_to_exp "6;;")
            "eval_s basic3";

    (* 6 * 6*)
    let test4 = str_to_exp "6*6;;" in
    let test4st = eval_s test4 (Env.empty ()) in
    let test4ex = extract_val test4st in
    unit_test (test4ex = str_to_exp "36;;")
            "eval_s basic4";
 
    (* (fun x -> x * x) (8 - 2) *)
    let test5s = str_to_exp "(fun x -> x * x) (8 - 2);;" in
    let test5t = eval_s test5s (Env.empty ()) in
    let test5ex = extract_val test5t in
    let test5 = str_to_exp "(fun x -> x * x) (8 - 2);;" 
        |> eval_s_help_test |> extract_val
    in
    unit_test (test5ex=str_to_exp "36;;")
        "eval_s basic5 no pipeline";
    unit_test (test5 = str_to_exp "36;;")
        "eval_s basic5";

    (*   let x = 3 + 5 in (fun x -> x * x) (x - 2) *)
    let test6 = str_to_exp "let x = 3 + 5 in (fun x -> x * x) (x - 2);;" 
        |> eval_s_help_test |> extract_val
    in
    unit_test (test6=str_to_exp"36;;")
        "eval_s basic6";

    (* let x = 51 in let x = 124 in x  *)
    let test7 = str_to_exp "let x = 51 in let x = 124 in x ;;"
        |> eval_s_help_test |> extract_val
    in
    unit_test (test7=str_to_exp"124;;")
        "eval_s basic7";

    (* let x = 6 in let y = 3 in x * y  *)
    let test8 = str_to_exp "let x = 6 in let y = 3 in x * y;;"
        |> eval_s_help_test |> extract_val
    in
    unit_test (test8=str_to_exp"18;;")
        "eval_s basic8";

    (* fun x -> x *2  *)
    let test9 = str_to_exp "fun x -> x *2;;"
        |> eval_s_help_test |> extract_val
    in
    unit_test (test9=str_to_exp"fun x -> x *2;;")
        "eval_s basic9";

    (* let x = 3 in fun x -> x * 2;; *)
    let test10 = str_to_exp "let x = 3 in fun x -> x * 2;;"
        |> eval_s_help_test |> extract_val
    in
    unit_test (test10=str_to_exp"fun x -> x * 2;;")
        "eval_s basic10";
    
    (* let f = fun x -> x * 2 in f 21 *)
    let test11 = str_to_exp "let f = fun x -> x * 2 in f 21;;"
        |> eval_s_help_test |> extract_val
    in
    unit_test (test11=str_to_exp"42;;")
        "eval_s basic11";
    
    (* let intofbool = fun b -> if b then 1 else 0 in intofbool true  *)
    let test12 = str_to_exp 
        "let intofbool = fun b -> if b then 1 else 0 in intofbool true;;"
        |> eval_s_help_test |> extract_val
    in
    unit_test (test12=str_to_exp"1;;")
        "eval_s basic12";

    (* let rec fact = fun n -> if n = 0 then 1 else n * fact (n-1) in fact 10  *)
    let test13 = str_to_exp 
        "let rec fact = fun n -> if n=0 then 1 else n * fact (n-1) in fact 10;;"
        |> eval_s_help_test |> extract_val
    in
    (* print_string (exp_to_abstract_string (str_to_exp 
        "let rec fact = fun n -> if n=0 then 1 else n * fact (n-1) in fact 10;;"));
    print_newline ();
    print_string (exp_to_concrete_string ((str_to_exp 
        "let rec fact = fun n -> if n=0 then 1 else n * fact (n-1) in fact 10;;"))); 
    print_newline (); *)
    print_string (exp_to_abstract_string test13) ;
    print_newline ();
    print_string (exp_to_concrete_string test13) ;
    print_newline ();
    unit_test (test13=str_to_exp"3628800;;")
        "eval_s basic13 LET REC!!!";
    
    (* DIFF FOR DYNAMIC!! *)
    (* let x = 2 in let f =  fun y -> x * y in let x = 1 in f 21 *)
    let test14 = str_to_exp 
        "let x = 2 in let f =  fun y -> x * y in let x = 1 in f 21;;"
        |> eval_s_help_test |> extract_val
    in
    unit_test (test14=str_to_exp"42;;")
        "eval_s basic14 DIFF FOR DYNAMIC";

     (* DIFF FOR DYNAMIC!! *)
    (* let x = 10 in let f = fun y -> fun z -> z * (x + y) in f 11 2*)
    let test15 = str_to_exp 
        "let x = 10 in let f = fun y -> fun z -> z * (x + y) in f 11 2;;"
        |> eval_s_help_test |> extract_val
    in
    unit_test (test15=str_to_exp"42;;")
        "eval_s basic15 DIFF FOR DYNAMIC";
;;

let eval_d_test ()=
    let eval_d_help_test (x : expr) = 
        eval_d x (Env.empty())
    in

      (* DIFF FOR DYNAMIC!! 42 in sub and lex, doesn't work in dyn
        this is a test i have to solve *)
    (* let x = 10 in let f = fun y -> fun z -> z * (x + y) in f 11 2 *)
    let _ = try (eval_d_help_test(str_to_exp 
        "let x = 10 in let f = fun y -> fun z -> z * (x + y) in f 11 2;;)"))
        with Evaluation.EvalError "variable unbound" -> 
            print_string "variable unbound passed \n"; Evaluation.Env.Val (Raise)
    in

    (* 42 *)
     let test2 = str_to_exp 
        "42;;"
        |> eval_d_help_test |> extract_val
    in
    unit_test (test2=str_to_exp"42;;")
        "eval_d basic2";

    (* 21*2 *)
     let test3 = str_to_exp 
        "21*2;;"
        |> eval_d_help_test |> extract_val
    in
    unit_test (test3=str_to_exp"42;;")
        "eval_d basic3";
    
    (* fun x -> x * 2 *)
     let test4 = str_to_exp 
        "fun x -> x * 2;;"
        |> eval_d_help_test |> extract_val
    in
    unit_test (test4=str_to_exp"fun x -> x * 2;;")
        "eval_d basic4";
    
    (* !! let x = 3 in fun x -> x*2 *)
    (* should return [function x -> [x*2]] where [{x ->3}] *)
    let test5 = str_to_exp 
        "let x = 3 in fun x -> x*2 ;;"
        |> eval_d_help_test
    in 
    print_string(Env.value_to_string ~printenvp:true test5 ^"\n");
    print_string(Env.value_to_string ~printenvp:true test5 ^"\n");

    (* let f = fun x -> x*2 in f 21 *)
    (* should return 42 *)
    let test6 = str_to_exp 
        "let f = fun x -> x*2 in f 21;;"
        |> eval_d_help_test |> extract_val
    in 
    unit_test (test6 = str_to_exp "42;;")
        "eval_d basic6";
    
    (* !!let intofbool = fun b -> if b then 1 else 0 in intofbool true;; *)
    (* should return 1 *)
    let test7 = str_to_exp 
        "let intofbool = fun b -> if b then 1 else 0 in intofbool true;;"
        |> eval_d_help_test |> extract_val
    in 
    print_string(exp_to_concrete_string test7 ^"\n");
    print_string(exp_to_abstract_string test7 ^"\n");
    unit_test (test7 = str_to_exp "1;;")
        "eval_d intofbool7";

    (* !! let rec fact = fun n -> if n = 0 then 1 else n * fact (n-1) in fact 10 *)
    (* should return 3628800*)
    let test8 = str_to_exp 
        "let rec fact = fun n -> if n = 0 then 1 else n * fact (n-1) in fact 10;;"
        |> eval_d_help_test |> extract_val
    in 
    print_string(exp_to_concrete_string test8 ^"\n");
    print_string(exp_to_abstract_string test8 ^"\n");
    unit_test (test8 = str_to_exp "3628800;;")
        "eval_d factorial";

    (*   let x = 2 in let f =  fun y -> x * y in let x = 1 in f 21;;*)
    (* should return 21 in dyn, but 42 in sub and lex *)
    let test9 = str_to_exp 
        "  let x = 2 in let f =  fun y -> x * y in let x = 1 in f 21;;"
        |> eval_d_help_test |> extract_val
    in 
    unit_test (test9 = str_to_exp "21;;")
        "eval_d basic9;";

    (* repaired unbound error *)
    (* let x = 10 in let f = fun y -> fun z -> z * (x + y) in let y = 12 in f 11 2;; *)
    (* should return 44 *)
    let test10 = str_to_exp 
        "let x = 10 in let f = fun y -> fun z -> z * (x + y) in let y = 12 in f 11 2;;"
        |> eval_d_help_test |> extract_val
    in 
    unit_test (test10 = str_to_exp "44;;")
        "eval_d basic10";
    ;;

(* 
    let intofbool = fun b -> if b then 1 else 0 in intofbool true;;
 *)

 (*  let rec fact n = if n = 0 then 1 else n * fact (n-1) in fact 10 ;;
    let rec fact = fun n -> if n = 0 then 1 else n * fact (n-1) in fact 10 ;;
        should output to 3628800
  *)
(* App (Fun (n, Conditional (Binop (Equals, Var (n), Num (0)), Num (1), Binop (Times, Var (n), App (Var (fact), Binop (Minus, Var (n), Num (1)))))), Num (10)) *)
  (* 
    let x = 2 in let f y = x * y in let x = 1 in f 21;;
    let x = 2 in let f =  fun y -> x * y in let x = 1 in f 21;;
        s should be 42
        d should be 21
        l should be 42

   *)

   (* 
        let x = 10 in let f y = fun z -> z * (x + y) in f 11 2;;
         let x = 10 in let f = fun y -> fun z -> z * (x + y) in f 11 2;;
        s 42
        d unbound
        l 42

let x = 10 in let f y = fun z -> z * (x + y) in let y = 12 in f 11 2;;
let x = 10 in let f = fun y -> fun z -> z * (x + y) in let y = 12 in f 11 2;;
        s 42
        d 44
        l 42
   
    *)
let test_all () = 
    subst_test ();
    eval_s_test ();
    (* eval_l_test(); make sure to test the 7:24 case that should eval to 42 *)
    eval_d_test ();;

let _ = test_all () ;;