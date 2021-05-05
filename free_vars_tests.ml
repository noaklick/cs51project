(* 
                          CS 51 Final Project
                                MiniML
                            Free Vars Testing
 *)
open Expr;;
open Miniml;;
open CS51Utils ;; 
open Absbook ;;


(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

let free_vars_test () =

    let test1 = str_to_exp "let x = 3 in let y = x in f x y;;" in
    unit_test (free_vars test1 =  (vars_of_list ["f"]))
        "freevars1 ";

    let test2 = str_to_exp "let x = x in let y = x in f x y;;" in
    unit_test (free_vars test2 =  (vars_of_list ["f"; "x"]))
        "freevars2 ";

    let test3 = str_to_exp "let x = y in let y = x in f x y;;" in
    unit_test (free_vars test3 =  (vars_of_list ["f"; "y"]))
        "freevars3";

    let test4 = str_to_exp "let x = fun y -> x in x;;" in
    unit_test (free_vars test4 =  (vars_of_list ["x"]))
        "freevars4";



;;

let _ = free_vars_test () ;;