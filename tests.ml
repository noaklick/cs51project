open Miniml;;
open Expr;;
let test1 = str_to_exp "x+1;;"
let test2 = subst "x" (Num (50)) test1;;
print_string (exp_to_abstract_string test2) ;
print_newline ();
print_string (exp_to_concrete_string test2);
print_newline ()