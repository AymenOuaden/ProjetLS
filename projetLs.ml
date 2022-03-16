
(* Q1 *)
type aexp =
  Var of string
  | Int of int
  | Plus of aexp*aexp
  | Mult of aexp*aexp
  | Sub of aexp*aexp ;;

(* Q2 *)    
Int 2;;

Plus(Int 2,Int 3);;
Sub(Int 2,Int 5);;
Mult(Int 3,Int 6);;

Plus(Int 2,Var "x");;
Mult(Int 4,Var "y");;
Mult(Int 3,Mult(Var "x",Var "x"));;
Mult(Int 5,Plus(Var "x",Mult(Int 7,Var "y")));;
Mult(Int 6,Plus(Var "x",Mult(Int 5,Mult(Var "y",Var "x"))));;

(* Q3 *)  
let rec aexp_to_string exp = match exp with 
| Var v -> v
| Int i -> string_of_int i
| Plus (first, second) ->  "(" ^ aexp_to_string first ^ " + " ^ aexp_to_string second ^ ")"
| Mult (first, second) ->  "(" ^ aexp_to_string first ^ " * " ^ aexp_to_string second ^ ")"
| Sub (first, second) ->  "(" ^ aexp_to_string first ^ " - " ^ aexp_to_string second ^ ")" ;;

let exp = Mult(Int 6,Plus(Var "x",Mult(Int 5,Mult(Var "y",Var "x"))));;
let exp_toString = aexp_to_string exp ;;
print_string (" Mult(Int 6,Plus(Var "x",Mult(Int 5,Mult(Var "y",Var "x"))))" ^ exp_toString);;
print_string ("\n");;

(* Q4 *) 
type valuation=
  Affect of string * int ;;






