(*   Les expressions arithmétiques *)
print_string("\n----------------------------   Les expressions arithmétiques   ----------------------------\n");;
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

print_string("\n------------ partie 1.1) question3 : ------------\n");;
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
print_string("\n------------ partie 1.1) question6 : ------------\n");;
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
print_string("\n------------ partie 1.1) question8 : ------------\n");;
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
 print_string("\n----------------------------  Les expressions booléennes   ----------------------------\n");;
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
let bexp1 = Vrai ;;
let bexp2 = Faux ;;
let bexp3 = Et(Vrai,Faux);;
let bexp4 = Non (Vrai);;
let bexp5 = Ou(Vrai,Faux);;

let bexp6 = Equal(Int 2, Int 4);;
let bexp7 = Equal(Plus(Int 3, Int 5), Mult(Int 2, Int 4));;
let bexp8 = Equal(Mult(Int 2, Var "x"), Plus(Var "y", Int 1));;

let bexp9 = EqualOrInf(Int 5, Int 7);;
let bexp10 = Et(EqualOrInf(Plus(Int 8, Int 9), Mult(Int 4, Int 5)),EqualOrInf(Plus(Int 3, Var "x"), Mult(Int 4, Var "y")));;


(* Q3-1  Fonction bexp_to_string *) 
let rec bexp_to_string exp = match exp with 
| Vrai -> "vrai"
| Faux -> "faux"
| Et (first, second) ->  "(" ^ bexp_to_string first ^ " et " ^ bexp_to_string second ^ ")"
| Ou (first, second) ->  "(" ^ bexp_to_string first ^ " ou " ^ bexp_to_string second ^ ")" 
| Non  bexp -> "!(" ^ bexp_to_string bexp ^ ")" 
| Equal (first, second) ->  "(" ^ aexp_to_string first ^ " = " ^ aexp_to_string second ^ ")"
| EqualOrInf (first, second) ->  "(" ^ aexp_to_string first ^ " <= " ^ aexp_to_string second ^ ")" ;;

(*Q3-2 Ecriture des chaines *)
print_string("\n------------ partie 1.2) question3 : ------------\n");;
print_string (" vrai  ==>  " ^ bexp_to_string bexp1 ^ " \n");;
print_string (" Faux  ==>  " ^ bexp_to_string bexp2 ^ " \n");;
print_string (" Et(Vrai,Faux)  ==>  " ^ bexp_to_string bexp3 ^ " \n");;
print_string (" Non (Vrai)  ==>  " ^ bexp_to_string bexp4 ^ " \n");;
print_string (" Ou(Vrai,Faux)  ==>  " ^ bexp_to_string bexp5 ^ " \n");;
print_string (" Equal(Int 2, Int 4)  ==>  " ^ bexp_to_string bexp6 ^ " \n");;
print_string (" Equal(Plus(Int 3, Int 5), Mult(Int 2, Int 4)) ==>  " ^ bexp_to_string bexp7 ^ " \n");;
print_string (" Equal(Mult(Int 2, Var 'x'), Plus(Var 'y', Int 1))  ==>  " ^ bexp_to_string bexp8 ^ " \n");;
print_string (" EqualOrInf(Int 5, Int 7)  ==>  " ^ bexp_to_string bexp9 ^ " \n");;
print_string (" Et(EqualOrInf(Plus(Int 8, Int 9), Mult(Int 4, Int 5)),EqualOrInf(Plus(Int 3, Var 'x'), Mult(Int 4, Var 'y')))  ==>  " ^ bexp_to_string bexp10 ^ " \n");;

(*  Q4  *)
let rec binterp exp varList = match exp with 
Vrai -> true
| Faux -> false
| Et (first, second) ->  binterp first varList && binterp second varList
| Ou (first, second) ->   binterp first varList || binterp second varList
| Non  bexp -> not (binterp bexp varList)
| Equal (first, second) ->   ainterp first varList == ainterp second varList
| EqualOrInf (first, second) ->  ainterp first varList <= ainterp second varList ;;  

(*  Q5  *)
print_string("\n------------ partie 1.2) question5 : ------------\n");;
let varList=(Val ("x", 7))::(Val ("y", 3))::[];;
let result = binterp bexp1 varList ;;
print_string("bexp1 ==> " ^string_of_bool(result)^"\n");;
let result = binterp bexp2 varList ;;
print_string("bexp2 ==> " ^string_of_bool(result)^"\n");;
let result = binterp bexp3 varList ;;
print_string("bexp3 ==> " ^string_of_bool(result)^"\n");;
let result = binterp bexp4 varList ;;
print_string("bexp4 ==> " ^string_of_bool(result)^"\n");;
let result = binterp bexp5 varList ;;
print_string("bexp5 ==> " ^string_of_bool(result)^"\n");;
let result = binterp bexp6 varList ;;
print_string("bexp6 ==> " ^string_of_bool(result)^"\n");;
let result = binterp bexp7 varList ;;
print_string("bexp7 ==> " ^string_of_bool(result)^"\n");;
let result = binterp bexp8 varList ;;
print_string("bexp8 ==> " ^string_of_bool(result)^"\n");;
let result = binterp bexp9 varList ;;
print_string("bexp9 ==> " ^string_of_bool(result)^"\n");;
let result = binterp bexp10 varList ;;
print_string("bexp10 ==> " ^string_of_bool(result)^"\n");;


(*  Les commandes du langage *)
print_string("\n----------------------------   Les commandes du langage   ----------------------------\n");;
(*  Q1  *)
type prog =
Skip
| Affect of string*aexp
| Sequence of prog*prog
| If of bexp*prog
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
| Affect (first,second) -> first ^ " := " ^ aexp_to_string second ^ "" 
| Sequence (first,second) -> prog_to_string first ^ " ; " ^ prog_to_string second ^ "\n" 
| If (first,second) -> "if (" ^ bexp_to_string first ^ ") then " ^  prog_to_string second ^ "\n" 
| If_Else (first,second,theard) -> "if (" ^ bexp_to_string first ^ ") then " ^  prog_to_string second ^ " else " ^ prog_to_string theard ^ " \n"
| Repeat (first,second) -> "repeat " ^ aexp_to_string first ^ " do " ^ prog_to_string second ^ " od \n";;

print_string("\n------------ partie 1.3) question3 : ------------\n");;
print_string("programme1 (to_string) ==> \n "^prog_to_string prog1^"\n");;
print_string("programme2 (to_string) ==> \n "^prog_to_string prog2);;
print_string("programme3 (to_string) ==> \n "^prog_to_string prog3);;
print_string("programme4 (to_string) ==> \n "^prog_to_string prog4);;

(*  Q4  *)
let rec selfcompose n f x = if (n<=0) then x else  selfcompose (n-1) f (f x) ;;

(*  Q5  *)
print_string("\n------------ partie 1.3) question5 : ------------\n");;
let fct x = x+2 ;;
let result = selfcompose 10 fct 0 ;;
print_string("aplication de la fonction x+2 10 fois avec selfcompose et x=0  ==> " ^ string_of_int(result) ^ "\n" );;

(*  Q6  *)
(* function to change the value of element in valuation by new value if is existing otherwise we add this variable to the list  *)
let rec changeVarValue var newVal varList = match varList with 
| Val (first,second) :: varList' ->  if ((compare first var)== 0 ) then Val(var,newVal)::varList' else Val (first,second)::(changeVarValue var newVal varList')
| [] -> Val(var,newVal) ::[];;

(* fonction that convert valuation to string  *)
let rec valuation_to_string varList = match varList with 
| Val (first,second) :: varList' -> first ^ " = " ^ string_of_int(second) ^ "  " ^ valuation_to_string varList'
| [] -> "\n";;


(* we created this function instead of selfcompose function because we can't execute repeat loop using selfcompose function  *)
let rec selfcomposeV2 n f prog varList = if (n<=0) then varList else let list=f prog varList in selfcomposeV2 (n-1) f prog list ;; 


let rec exec prog varList = match prog with 
| Skip -> varList 
| Affect (first,second) -> changeVarValue first (ainterp second varList) varList 
| Sequence (first,second) -> exec second (exec first varList)
| If (first,second) -> if(binterp first varList) then exec second varList else varList
| If_Else (first,second,theard) -> if(binterp first varList) then exec second varList else exec theard varList
| Repeat (first,second) -> selfcomposeV2 (ainterp first varList) exec second varList
;;


(*  Q8  *)
print_string("\n------------ partie 1.3) question7 : ------------\n");;

(* Factorielle *)
let progFactorielle = If_Else(EqualOrInf(Var "n", Int 0), Affect("result", Int 1),Sequence(Affect("result", Int 1),Repeat(Var "n",Sequence(Affect("result", Mult(Var "result", Var "n")),Affect("n", Sub(Var "n",Int 1)) ))) );;
let varList=(Val ("n", 5))::[];;
print_string("Factorielle ==>  " ^ (valuation_to_string varList));;
let varListResult=exec progFactorielle varList ;;
print_string("resultat = "^string_of_int(getVarValue "result" varListResult) ^ "\n");;

(* Fibonacci *)
let progFibonacci = Sequence(Affect("a",Int 0), Sequence(Affect("b",Int 1) ,Repeat(Var "n", Sequence(Sequence(Affect("a'",Var "a") ,Affect("a",Var "b") ) , Affect("b",Plus(Var "a'", Var "b"))) ) ));;
let varList=(Val ("n", 8))::[];;
print_string("Fibonacci ==>  " ^ (valuation_to_string varList));;
let varListResult=exec progFibonacci varList ;;
print_string("resultat = "^string_of_int(getVarValue "a" varListResult) ^ "\n");;



(*  Triplets de Hoare et validité *)
print_string("\n----------------------------   Triplets de Hoare et validité   ----------------------------\n \n");;

(*  4.1 formules de la logiques des propositions  *)
(*  Q1  *)
type tprop =
Vrai
  | Faux
  | Et of tprop*tprop
  | Ou of tprop*tprop
  | Non of tprop
  | Equal of aexp*aexp
  | EqualOrInf of aexp*aexp 
  | Implique of tprop*tprop  ;;

(*  Q2  *)

let tprop1 = Vrai;;
let tprop2 = Et(Vrai,Faux);;
let tprop3 = Non Vrai;;
let tprop4 = Ou(Vrai,Faux);;
let tprop5 = Implique(Faux,Ou(Vrai,Faux)) ;;

let tprop6 =  Equal(Int 2,Int 4);;
let tprop7 =  Equal(Plus(Int 3, Int 5),Mult(Int 2, Int 4));;
let tprop8 =  Equal(Mult(Int 2, Var "x"),Plus(Var "y", Int 1));;

let tprop9 = EqualOrInf(Plus(Int 3, Var "x") ,Mult(Int 4, Var "y"));;
let tprop10 = Et(EqualOrInf(Int 5 ,Int 7 ),EqualOrInf(Plus( Int 8, Int 9 ) ,Mult(Int 4, Int 5)));;
let tprop11 = Implique( Equal(Var "x",Int 1),EqualOrInf(Var "y",Int 0));;

(*  Q3  *)
(* Q3-1  Fonction bexp_to_string *) 
let rec prop_to_string prop  = match prop with
| Vrai -> "vrai"
| Faux -> "faux"
| Ou(first,second) -> "(" ^ prop_to_string first ^ " ou " ^ prop_to_string second ^ ")"
| Et(first,second) -> "(" ^ prop_to_string first ^ " et " ^ prop_to_string second ^ ")"
| Non  p -> "!(" ^ prop_to_string p ^ ")" 
| Equal (first, second) ->  "(" ^ aexp_to_string first ^ " = " ^ aexp_to_string second ^ ")"
| EqualOrInf (first, second) ->  "(" ^ aexp_to_string first ^ " <= " ^ aexp_to_string second ^ ")" 
| Implique (first, second) -> "(" ^ prop_to_string first ^ " implique " ^ prop_to_string second ^ ")" ;;

(*Q3-2 Ecriture des chaines *)
print_string("\n------------ partie 4.1) question3 : ------------\n");;
print_string (" vrai  ==>  " ^ prop_to_string tprop1 ^ " \n");;
print_string (" Et(Vrai,Faux)  ==>  " ^ prop_to_string tprop2 ^ " \n");;
print_string (" Non Vrai  ==>  " ^ prop_to_string tprop3 ^ " \n");;
print_string (" Ou(Vrai,Faux)  ==>  " ^ prop_to_string tprop4 ^ " \n");;
print_string (" Implique(Faux,Ou(Vrai,Faux))  ==>  " ^ prop_to_string tprop5 ^ " \n");;
print_string (" Equal(Int 2,Int4)  ==>  " ^ prop_to_string tprop6 ^ " \n");;
print_string (" Equal(Plus(Int 3, Int 5),Mult(Int 2, Int 4))  ==>  " ^ prop_to_string tprop7 ^ " \n");;
print_string (" Equal(Mult(Int 4, Var x),Plus(Var y, Int 1))  ==>  " ^ prop_to_string tprop8 ^ " \n");;
print_string (" EqualOrInf(Plus(Int 3, Var x) ,Mult(Int 4, Var y))  ==>  " ^ prop_to_string tprop9 ^ " \n");;
print_string (" Et(EqualOrInf(Int 5 ,Int 7 ),EqualOrInf(Plus( Int 8, Int 9 ) ,Mult(Int 4, Int 5)))  ==>  " ^ prop_to_string tprop10 ^ " \n");;
print_string (" Implique( Equal(Var x,Int 1),EqualOrInf(Var y,Int 0))  ==>  " ^ prop_to_string tprop11 ^ " \n");;


(*  4.2 Interprétation  *)

(*  Q4  *)

let rec pinterp prop varList = match prop with 
| Vrai -> true
| Faux -> false
| Ou(first,second) ->  pinterp first varList || pinterp second varList
| Et(first,second) -> pinterp first varList && pinterp second varList
| Non  p -> not (pinterp p varList) 
| Equal (first, second) ->  ainterp first varList == ainterp second varList
| EqualOrInf (first, second) ->  ainterp first varList <= ainterp second varList
| Implique (first, second) -> not( pinterp first varList) || ( pinterp second varList)  ;;

(*  Q5  *)
print_string("\n------------ partie 1.4) question5 : ------------\n");;
let varList=(Val ("x", 7))::(Val ("y", 3))::[];;
let result = pinterp tprop1 varList ;;
print_string("tprop1 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop2 varList ;;
print_string("tprop2 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop3 varList ;;
print_string("tprop3 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop4 varList ;;
print_string("tprop4 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop5 varList ;;
print_string("tprop5 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop6 varList ;;
print_string("tprop6 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop7 varList ;;
print_string("tprop7 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop8 varList ;;
print_string("tprop8 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop9 varList ;;
print_string("tprop9 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop10 varList ;;
print_string("tprop10 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop11 varList ;;
print_string("tprop11 ==> " ^string_of_bool(result)^"\n");;

(*  4.3 Substitutions  *)
(*  Q6  *)
let rec psubst tprop var aexp =match tprop with 
| Vrai -> Vrai
| Faux -> Faux
| Ou(first,second) ->Ou((psubst first var aexp ),(psubst second var aexp ))
| Et(first,second) -> Et((psubst first var aexp ),(psubst second var aexp ))
| Non tprop' -> Non (psubst tprop' var aexp )
| Equal(first,second) -> Equal((asubst first var aexp ),(asubst second var aexp))
| EqualOrInf(first,second) -> EqualOrInf ((asubst first var aexp ),(asubst second var aexp))
| Implique(first,second) -> Implique((psubst first var aexp),(psubst second var aexp)) ;;


(*  Q7  *)
print_string("\n------------ partie 1.4.3) question7 : ------------\n");;
let aexpX=Mult(Int 3, Var "y");;
let aexpY=Plus(Var "k", Int 2);;

let resultaexp = psubst tprop8 "x" aexpX ;;
let resultaexp = psubst resultaexp "y" aexpY ;;
print_string("formule logique 8 = "^prop_to_string tprop8 ^ " ==>  " ^ prop_to_string resultaexp ^ "\n");;
let resultaexp = psubst tprop9 "x" aexpX ;;
let resultaexp = psubst resultaexp "y" aexpY ;;
print_string("formule logique 9 = "^prop_to_string tprop9 ^ " ==>  " ^ prop_to_string resultaexp ^ "\n");;
let resultaexp = psubst tprop11 "x" aexpX ;;
let resultaexp = psubst resultaexp "y" aexpY ;;
print_string("formule logique 11 = "^prop_to_string tprop11 ^ " ==>  " ^ prop_to_string resultaexp ^ "\n");;


(*  Les triplets de Hoare  *)
print_string("\n----------------------------   Les triplets de Hoare   ----------------------------\n");;
(*  Q8  *)
type hoare_triple =
Triplet of tprop*prog*tprop;;

(*  Q9  *)
let hoare_triple1 = Triplet( Equal(Var "x", Int 2), Skip, Equal( Var "x", Int 2));;
let hoare_triple2 = Triplet( Equal(Var "x", Int 2), Affect("x", Int 3 ), EqualOrInf(Var "x", Int 3));;
let hoare_triple3 = Triplet( Vrai, If_Else(EqualOrInf(Var "x", Int 0),  Affect("r", Sub(Int 0, Var "x")),Affect("r", Var "x")),EqualOrInf(Int 0, Var "r"));;
let hoare_triple4 = Triplet( Et(Equal(Var "in",Int 5),Equal(Var "out", Int 1)),progFactorielle,Et(Equal(Var "in",Int 0),Equal(Var "out", Int 120)));;

(*  Q10  *)
let rec htvalid_test h_triple varList =match  h_triple with 
| Triplet(prec,prog,postc) -> (pinterp prec varList) && (pinterp postc (exec prog varList ));;

let varList=(Val ("x", 2))::(Val ("y", 3))::[];;
print_string("result ==> " ^string_of_bool(htvalid_test hoare_triple1 varList )^"\n");;



(*  Un (mini) prouveur en logique de Hoare *)
print_string("\n----------------------------   Un (mini) prouveur en logique de Hoare  ----------------------------\n \n");;

(*  Les buts de preuves et le langage des tactiques *)
(*  Q1  *)
type context=
Context of (string*tprop) list;;

type conclusion =
 ConclusionProp of tprop 
| ConclusionHoare of hoare_triple;;

type goal =
|Goal of  context*conclusion;;

(*  Q1  *)
let p :tprop = Vrai;;
let q :tprop = Faux;;
let r :tprop = Vrai;;

let goal1 = Goal(Context([("H",Implique(Ou(p,q),r));("H2",p)]),ConclusionProp(Ou(p,q)));;
let goal2 = Goal(Context [], ConclusionHoare(Triplet( Equal(Var "x", Int (-3)), If_Else(EqualOrInf(Var "x", Int 0),  Affect("x", Sub(Int 0, Var "x")),Affect("x", Var "x")),Equal(Var "x", Int 3))));;


(*  Q3  *)

let print_hoare_triple hoare_triple = match hoare_triple with 
 Triplet(pre, prog, post) -> "{ " ^ prop_to_string(pre)^" }\n "^prog_to_string(prog)^"{ "^prop_to_string(post)^" }\n";;

let rec print_context context_exp = match context_exp with 
|Context [] -> "============== \n"
|Context ( (name,prop)::rest ) -> name ^ " : " ^ (prop_to_string prop) ^ "\n" ^ print_context (Context rest)

let print_conclusion conclusion_exp =  match conclusion_exp with
|ConclusionProp prop -> prop_to_string prop 
|ConclusionHoare hoare -> print_hoare_triple hoare ;;

let print_goal goal_exp = match goal_exp with
|Goal(context,conclusion) -> print_context context  ^ print_conclusion conclusion ^ "\n \n" ;;               

print_string("\n------------ partie 2.1.1) question3 : ------------\n");;
print_string(print_goal goal1);;
print_string(print_goal goal2);;


let fresh_ident =
let prefix = " H " and count = ref 0
in function () -> ( count := ! count + 1 );


(*  Q4  *)


(* normalement kayan la solution ta3hom fi github nverifyiw berk ida s7i7a 
   machi des fonction hado qst reponse *)


(*  Q5  *)


(* normalement kayan la solution ta3hom fi github nverifyiw berk ida s7i7a 
   machi des fonction hado qst reponse *)


(*  Q6  *)
type tactic =
  And_Intro
  | Or_Intro_1
  | Or_Intro_2
  | Impl_Intro
  | Not_Intro
  |And_Elim_1 of string
  |And_Elim_2 of string 
  |Or_Elim of string 
  |Impl_Elim of string
  |Not_Elim of string*string
  |Exact of string 
  |Assume of tprop
  |HSkip 
  |HAssign
  |HIf
  |HRepeat of string 
  |HCons of tprop*tprop
  |HSEq of tprop*tprop;;

(*   Appliquer une tactique à un but  *)
 
(* Q1 *)





 
(* Q2 *)
