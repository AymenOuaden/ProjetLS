
(* Q1 *)
type aexp =
  Var of string
  | Int of int
  | Plus of aexp*aexp
  | Mult of aexp*aexp
  | Sub of aexp*aexp ;;

(* Q2 *)    
Int 2;;

let aexp1 = Plus(Int 2,Int 3);;
let aexp2 = Sub(Int 2,Int 5);;
let aexp3 = Mult(Int 3,Int 6);;

let aexp4 = Plus(Int 2,Var "x");;
let aexp5 = Mult(Int 4,Var "y");;
let aexp6 = Mult(Int 3,Mult(Var "x",Var "x"));;
let aexp7 = Mult(Int 5,Plus(Var "x",Mult(Int 7,Var "y")));;
let aexp8 = Mult(Int 6,Plus(Var "x",Mult(Int 5,Mult(Var "y",Var "x"))));;

(* Q3 *)  
let rec aexp_to_string exp = match exp with 
| Var var -> var
| Int i -> string_of_int i
| Plus (first, second) ->  "(" ^ aexp_to_string first ^ " + " ^ aexp_to_string second ^ ")"
| Mult (first, second) ->  "(" ^ aexp_to_string first ^ " * " ^ aexp_to_string second ^ ")"
| Sub (first, second) ->  "(" ^ aexp_to_string first ^ " - " ^ aexp_to_string second ^ ")" ;;

let exp = Mult(Int 6,Plus(Var "x",Mult(Int 5,Mult(Var "y",Var "x"))));;
let exp_toString = aexp_to_string exp ;;
print_string (" Mult(Int 6,Plus(Var x,Mult(Int 5,Mult(Var y,Var x))))    ===>   " ^ exp_toString  ^ "\n" );;

(* Q4 *) 
type valuation=
Val of string * int ;;

(* Q5 *) 

let rec getVarValue var varList = match varList with 
| Val (first,second) :: varList' ->  if ((compare first var)== 0 ) then second  else getVarValue var varList'
| [] -> failwith "no matching variable found ";;

let rec ainterp exp varList = match exp with 
| Var var -> getVarValue var varList
| Int i -> i
| Plus (first, second) ->  ainterp first varList + ainterp second varList
| Mult (first, second) ->  ainterp first varList * ainterp second varList
| Sub (first, second) ->  ainterp first varList - ainterp second varList ;;

(* Q6 *)

let varList=(Val ("x", 5))::(Val ("y", 9))::[];;
let result = ainterp aexp1 varList ;;
print_string("expresion1 = "^aexp_to_string aexp1 ^" ==>  result  =  " ^ string_of_int(result) ^ "\n");;
let result = ainterp aexp2 varList ;;
print_string("expresion2 = "^aexp_to_string aexp2 ^" ==>  result  =  " ^ string_of_int(result) ^ "\n");;
let result = ainterp aexp3 varList ;;
print_string("expresion3 = "^aexp_to_string aexp3 ^" ==>  result  =  " ^ string_of_int(result) ^ "\n");;
let result = ainterp aexp4 varList ;;
print_string("expresion4 = "^aexp_to_string aexp4 ^" ==>  result  =  " ^ string_of_int(result) ^ "\n");;
let result = ainterp aexp5 varList ;;
print_string("expresion5 = "^aexp_to_string aexp5 ^" ==>  result  =  " ^ string_of_int(result) ^ "\n");;
let result = ainterp aexp6 varList ;;
print_string("expresion6 = "^aexp_to_string aexp6 ^" ==>  result  =  " ^ string_of_int(result) ^ "\n");;
let result = ainterp aexp7 varList ;;
print_string("expresion7 = "^aexp_to_string aexp7 ^" ==>  result  =  " ^ string_of_int(result) ^ "\n");;
let result = ainterp aexp8 varList ;;
print_string("expresion8 = "^aexp_to_string aexp8 ^" ==>  result  =  " ^ string_of_int(result) ^ "\n");;

(* Q7 *)
let rec asubst exp1 var exp2 = match exp1 with 
| Var v -> if ((compare v var)== 0 ) then exp2 else exp1
| Int i -> exp1
| Plus (first, second) ->  Plus((asubst first var exp2), (asubst second var exp2))
| Mult (first, second) ->  Mult((asubst first var exp2), (asubst second var exp2))
| Sub (first, second) ->  Sub((asubst first var exp2), (asubst second var exp2)) ;;

(* Q8 *)
print_string("Question 8 \n");;
let aexpX=Int 7;;
let aexpY=Plus(Var "z", Int 2);;

let resultaexp = asubst aexp4 "x" aexpX ;;
print_string("expresion4 = "^aexp_to_string aexp4 ^ " ==>  " ^ aexp_to_string resultaexp ^ "\n");;
let resultaexp = asubst aexp5 "y" aexpY ;;
print_string("expresion5 = "^aexp_to_string aexp5 ^ " ==>  " ^ aexp_to_string resultaexp ^ "\n");;
let resultaexp = asubst aexp6 "x" aexpX ;;
print_string("expresion6 = "^aexp_to_string aexp6 ^ " ==>  " ^ aexp_to_string resultaexp ^ "\n");;
let resultaexp = asubst aexp7 "x" aexpX ;;
let resultaexp = asubst resultaexp "y" aexpY ;;
print_string("expresion7 = "^aexp_to_string aexp7 ^ " ==>  " ^ aexp_to_string resultaexp ^ "\n");;
let resultaexp = asubst aexp8 "x" aexpX ;;
let resultaexp = asubst resultaexp "y" aexpY ;;
print_string("expresion8 = "^aexp_to_string aexp8 ^ " ==>  " ^ aexp_to_string resultaexp ^ "\n");;

(*  Les expressions booléennes *)
(* Q1  Définition du type*)
type bexp =
  Vrai
  | Faux
  | Et of bexp*bexp
  | Ou of bexp*bexp
  | Non of bexp
  | Equal of aexp*aexp
  | EqualOrInf of aexp*aexp ;;

(* Q2 Ecriture des expressions*)
let exp1 = Vrai ;;
let exp2 = Faux ;;
let exp3 = Et(Vrai,Faux);;
let exp4 = Non (Vrai);;
let exp5 = Ou(Vrai,Faux);;

let exp6 = Equal(Int 2, Int 4);;
let exp7 = Equal(Plus(Int 3, Int 5), Mult(Int 2, Int 4));;
let exp8 = Equal(Mult(Int 2, Var "x"), Plus(Var "y", Int 1));;

let exp9 = EqualOrInf(Int 5, Int 7);;
let exp10 = Et(EqualOrInf(Plus(Int 8, Int 9), Mult(Int 4, Int 5)),EqualOrInf(Plus(Int 3, Var "x"), Mult(Int 4, Var "y")));;


(* Q3-1  Fonction bexp_to_string *) 
let rec bexp_to_string exp = match exp with 
| Vrai -> "vrai"
| Faux -> "faux"
| Et (first, second) ->  "(" ^ bexp_to_string first ^ " et " ^ bexp_to_string second ^ ")"
| Ou (first, second) ->  "(" ^ bexp_to_string first ^ " ou " ^ bexp_to_string second ^ ")" 
| Non  bexp -> "(" ^ bexp_to_string bexp ^ ")" 
| Equal (first, second) ->  "(" ^ aexp_to_string first ^ " = " ^ aexp_to_string second ^ ")"
| EqualOrInf (first, second) ->  "(" ^ aexp_to_string first ^ " <= " ^ aexp_to_string second ^ ")" ;;

(*Q3-2 Ecriture des chaines *)
print_string (" vrai  ==>  " ^ bexp_to_string exp1 ^ " \n");;
print_string (" Faux  ==>  " ^ bexp_to_string exp2 ^ " \n");;
print_string (" Et(Vrai,Faux)  ==>  " ^ bexp_to_string exp3 ^ " \n");;
print_string (" Non (Vrai)  ==>  " ^ bexp_to_string exp4 ^ " \n");;
print_string (" Ou(Vrai,Faux)  ==>  " ^ bexp_to_string exp5 ^ " \n");;
print_string (" Equal(Int 2, Int 4)  ==>  " ^ bexp_to_string exp6 ^ " \n");;
print_string (" Equal(Plus(Int 3, Int 5), Mult(Int 2, Int 4)) ==>  " ^ bexp_to_string exp7 ^ " \n");;
print_string (" Equal(Mult(Int 2, Var 'x'), Plus(Var 'y', Int 1))  ==>  " ^ bexp_to_string exp8 ^ " \n");;
print_string (" EqualOrInf(Int 5, Int 7)  ==>  " ^ bexp_to_string exp9 ^ " \n");;
print_string (" Et(EqualOrInf(Plus(Int 8, Int 9), Mult(Int 4, Int 5)),EqualOrInf(Plus(Int 3, Var 'x'), Mult(Int 4, Var 'y')))  ==>  " ^ bexp_to_string exp10 ^ " \n");;

(*  Q4  *)
let rec binterp exp varList = match exp with 
Vrai -> true
| Faux -> false
| Et (first, second) ->  binterp first varList && binterp second varList
| Ou (first, second) ->   binterp first varList || binterp second varList
| Non  bexp -> (binterp bexp varList)
| Equal (first, second) ->   ainterp first varList == ainterp second varList
| EqualOrInf (first, second) ->  ainterp first varList <= ainterp second varList ;;  

(*  Q5  *)
let varList=(Val ("x", 5))::(Val ("y", 9))::[];;
let result = binterp exp6 varList ;;
print_string(string_of_bool(result)^"\n");;


(*  Les commandes du langage *)
(*  Q1  *)
type prog =
Skip
| Affect of string*aexp
| Sequence of prog*prog
| IF of bexp*prog
| If_Else of bexp*prog*prog
| Repeat of aexp*prog ;;

(*  Q2  *)
let prog1 = Affect ("y",Int 7);; 
let prog2 = Sequence ((Affect ("z",Plus(Int 3, Int 4))),((Affect ("x",Mult(Int 2, Var "x")))));;
let prog3 = Sequence ((Affect ("n",Int 3)),If_Else((EqualOrInf(Var "n",Int 4)),(Affect("n",Plus(Mult(Int 2,Var "n"),Int 3))),(Affect("n",Plus(Var "n",Int 1)))));;
let prog4 = Repeat (Int 10,Affect("x",Plus(Var "x",Int 1)));;

(*  Q3  *)

let rec prog_to_string prog = match prog with 
| Skip -> "skip " 
| Affect (first,second) -> first ^ " := " ^ aexp_to_string second ^ " "
| Sequence (first,second) -> prog_to_string first ^ " ; " ^ prog_to_string second ^ "\n" 
| IF (first,second) -> "if (" ^ bexp_to_string first ^ ") then " ^  prog_to_string second ^ "\n" 
| If_Else (first,second,theard) -> "if (" ^ bexp_to_string first ^ ") then " ^  prog_to_string second ^ " else " ^ prog_to_string theard ^ " \n"
| Repeat (first,second) -> "repeat " ^ aexp_to_string first ^ " do " ^ prog_to_string second ^ " od \n";;


print_string("programme1 (to_string) ==> \n "^prog_to_string prog1^"\n");;
print_string("programme2 (to_string) ==> \n "^prog_to_string prog2^"\n");;
print_string("programme3 (to_string) ==> \n "^prog_to_string prog3^"\n");;
print_string("programme4 (to_string) ==> \n "^prog_to_string prog4^"\n");;

(*  Q4  *)
