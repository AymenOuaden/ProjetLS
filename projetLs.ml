(*
Projet Logiciels sûrs : Un interpréteur pour un langage impératif et un prouveur en logique de Hoare
—Master Informatique, 1ière année, 2ème semestre, 2021-2022— 

Réalisé par : Islem CHEKLAT - Aymen OUADEN - Abel AMOUH

Language : OCAML

*)

(*Partie 1 :  Un interpréteur pour un langage impératif*)
print_string("\n----------------------------   Partie 1 :  Un interpréteur pour un langage impératif   ----------------------------\n");;

(* 1.1  Les expressions arithmétiques *)
print_string("\n \n 1.1 Les expressions arithmétiques \n");;

(* 1.1.1 Syntaxe abstraite *)
print_string("\n   1.1.1 Syntaxe abstraite  \n");;

(* Q1 : Type aexp définit la syntaxe abstraite des expressions arithmétiques du notre langage*)
type aexp =
  Var of string
  | Int of int
  | Plus of aexp*aexp
  | Mult of aexp*aexp
  | Sub of aexp*aexp ;;

(* Q2 : Ecriture des expressions *)    
let aexp0 = Int 2;;

let aexp1 = Plus(Int 2,Int 3);;
let aexp2 = Sub(Int 2,Int 5);;
let aexp3 = Mult(Int 3,Int 6);;

let aexp4 = Plus(Int 2,Var "x");;
let aexp5 = Mult(Int 4,Var "y");;
let aexp6 = Mult(Int 3,Mult(Var "x",Var "x"));;
let aexp7 = Mult(Int 5,Plus(Var "x",Mult(Int 7,Var "y")));;
let aexp8 = Mult(Int 6,Plus(Var "x",Mult(Int 5,Mult(Var "y",Var "x"))));;

(* Q3-1 : Ecriture de la fonction aexp_to_string qui transforme une expression arithmétique en une chaîne de caractères complètement parenthésée*)  
let rec aexp_to_string exp = match exp with 
| Var var -> var
| Int i -> string_of_int i
| Plus (first, second) ->  "(" ^ aexp_to_string first ^ " + " ^ aexp_to_string second ^ ")"
| Mult (first, second) ->  "(" ^ aexp_to_string first ^ " * " ^ aexp_to_string second ^ ")"
| Sub (first, second) ->  "(" ^ aexp_to_string first ^ " - " ^ aexp_to_string second ^ ")" ;;

(*Q3-2 : Affichage des chaines de caractères de la question 2*)
print_string("\n     Q3-2 : Affichage des chaines de caractères arithmétiques de la question 2 \n \n");;
print_string ("       Int 2    ===>   " ^   aexp_to_string aexp0  ^ "\n" );;
print_string ("       Plus(Int 2,Int 3)    ===>   " ^aexp_to_string aexp1  ^ "\n" );;
print_string ("       Sub(Int 2,Int 5)    ===>   " ^aexp_to_string aexp2  ^ "\n" );;
print_string ("       Mult(Int 3,Int 6)    ===>   " ^aexp_to_string aexp3  ^ "\n" );;
print_string ("       Plus(Int 2,Var 'x')    ===>   " ^aexp_to_string aexp4  ^ "\n" );;
print_string ("       Mult(Int 4,Var 'y')   ===>   " ^aexp_to_string aexp5  ^ "\n" );;
print_string ("       Mult(Int 3,Mult(Var 'x',Var 'x'))   ===>   " ^aexp_to_string aexp6  ^ "\n" );;
print_string ("       Mult(Int 5,Plus(Var 'x',Mult(Int 7,Var 'y')))   ===>   " ^aexp_to_string aexp7  ^ "\n" );;
print_string ("       Mult(Int 6,Plus(Var 'x',Mult(Int 5,Mult(Var 'y',Var 'x'))))   ===>   " ^aexp_to_string aexp8  ^ "\n" );;

(* 1.1.2 Interprétation *)

print_string("\n   1.1.2 Interprétation  \n");;

(* Q4 : Type valuation *) 
type valuation=
Val of (string * int) list ;;

(* Q5 *)
(*Q5-* : Retourner la valeur d'une variable *)
let rec getVarValue var valuation = match valuation with 
| Val ((nom,value)::val') ->  if ((compare nom var)== 0 ) then value  else getVarValue var (Val val')
| Val([]) -> failwith "no matching variable found ";;

(* La fonction ainterp qui renvoie la valeur à partir d'une expression et une valuation*)
let rec ainterp exp valuation = match exp with 
| Var var -> getVarValue var valuation
| Int i -> i
| Plus (first, second) ->  ainterp first valuation + ainterp second valuation
| Mult (first, second) ->  ainterp first valuation * ainterp second valuation
| Sub (first, second) ->  ainterp first valuation - ainterp second valuation ;;

(*Q6 : Evaluation via ainterp sachant que x=5 et y=9*)
print_string("\n     Q6 : Evaluation via ainterp sachant que x=5 et y=9  \n \n");;
let valuation1 =Val([("x", 5);("y", 9)]);;
let result1 = ainterp aexp1 valuation1 ;;
print_string("      expresion1 = "^aexp_to_string aexp1 ^" ==>  result1  =  " ^ string_of_int(result1) ^ "\n");;
let result2 =ainterp aexp2 valuation1 ;;
print_string("      expresion2 = "^aexp_to_string aexp2 ^" ==>  result2  =  " ^ string_of_int(result2) ^ "\n");;
let result3 =ainterp aexp3 valuation1 ;;
print_string("      expresion3 = "^aexp_to_string aexp3 ^" ==>  result3  =  " ^ string_of_int(result3) ^ "\n");;
let result4 = ainterp aexp4 valuation1 ;;
print_string("      expresion4 = "^aexp_to_string aexp4 ^" ==>  result4  =  " ^ string_of_int(result4) ^ "\n");;
let result5 = ainterp aexp5 valuation1 ;;
print_string("      expresion5 = "^aexp_to_string aexp5 ^" ==>  result5  =  " ^ string_of_int(result5) ^ "\n");;
let result6 = ainterp aexp6 valuation1 ;;
print_string("      expresion6 = "^aexp_to_string aexp6 ^" ==>  result6  =  " ^ string_of_int(result6) ^ "\n");;
let result7 = ainterp aexp7 valuation1 ;;
print_string("      expresion7 = "^aexp_to_string aexp7 ^" ==>  result7  =  " ^ string_of_int(result7) ^ "\n");;
let result8 = ainterp aexp8 valuation1 ;;
print_string("      expresion8 = "^aexp_to_string aexp8 ^" ==>  result8  =  " ^ string_of_int(result8) ^ "\n");;

(* 1.1.1 Syntaxe abstraite *)
print_string("\n   1.1.3 Substitutions  \n");;

(* Q7 : La fonction asubst *)
let rec asubst exp1 var exp2 = match exp1 with 
| Var v -> if ((compare v var)== 0 ) then exp2 else exp1
| Int i -> exp1
| Plus (argument1, argument2) ->  Plus((asubst argument1 var exp2), (asubst argument2 var exp2))
| Mult (argument1, argument2) ->  Mult((asubst argument1 var exp2), (asubst argument2 var exp2))
| Sub (argument1, argument2) ->  Sub((asubst argument1 var exp2), (asubst argument2 var exp2)) ;;

(*Q8 : Affichage via "asubst et aexp_to_string" sachant que x=7 et y=z+2*)
print_string("\n     Q8 : Affichage via 'asubst et aexp_to_string' sachant que x=7 et y=z+2  \n \n");;
let aexpX=Int 7;;
let aexpY=Plus(Var "z", Int 2);;

let resultaexp4 = asubst aexp4 "x" aexpX ;;
print_string("      expresion4 = "^aexp_to_string aexp4 ^ " ==>  " ^ aexp_to_string resultaexp4 ^ "\n");;
let resultaexp5 = asubst aexp5 "y" aexpY ;;
print_string("      expresion5 = "^aexp_to_string aexp5 ^ " ==>  " ^ aexp_to_string resultaexp5 ^ "\n");;
let resultaexp6 = asubst aexp6 "x" aexpX ;;
print_string("      expresion6 = "^aexp_to_string aexp6 ^ " ==>  " ^ aexp_to_string resultaexp6 ^ "\n");;
let resultaexp7 = asubst aexp7 "x" aexpX ;;
let resultaexp7 = asubst resultaexp7 "y" aexpY ;;
print_string("      expresion7 = "^aexp_to_string aexp7 ^ " ==>  " ^ aexp_to_string resultaexp7 ^ "\n");;
let resultaexp8 = asubst aexp8 "x" aexpX ;;
let resultaexp8 = asubst resultaexp8 "y" aexpY ;;
print_string("      expresion8 = "^aexp_to_string aexp8 ^ " ==>  " ^ aexp_to_string resultaexp8 ^ "\n");;

(* 1.2  Les expressions arithmétiques *)
print_string("\n \n 1.2 Les expressions Booléennes \n");;

(* 1.2.1 Syntaxe abstraite *)
print_string("\n   1.2.1 Syntaxe abstraite  \n");;

(* Q1 : Type bexp définit la syntaxe abstraite des expressions booléennes du notre langage*)
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

(* Q3-1 : Ecriture de la fonction bexp_to_string qui transforme une expression booléennes en une chaîne de caractères complètement parenthésée*)  
let rec bexp_to_string exp = match exp with 
| Vrai -> "vrai"
| Faux -> "faux"
| Et (argument1, argument2) ->  "(" ^ bexp_to_string argument1 ^ " et " ^ bexp_to_string argument2 ^ ")"
| Ou (argument1, argument2) ->  "(" ^ bexp_to_string argument1 ^ " ou " ^ bexp_to_string argument2 ^ ")" 
| Non  argument1 -> "!(" ^ bexp_to_string argument1 ^ ")" 
| Equal (argument1, argument2) ->  "(" ^ aexp_to_string argument1 ^ " = " ^ aexp_to_string argument2 ^ ")"
| EqualOrInf (argument1, argument2) ->  "(" ^ aexp_to_string argument1 ^ " <= " ^ aexp_to_string argument2 ^ ")" ;;

(*Q3-2 : Affichage des chaines de caractères de la question 2*)
print_string("\n     Q3-2 : Affichage des chaines de caractères booléennes de la question 2 \n \n");;
print_string ("       vrai  ==>  " ^ bexp_to_string bexp1 ^ " \n");;
print_string ("       Faux  ==>  " ^ bexp_to_string bexp2 ^ " \n");;
print_string ("       Et(Vrai,Faux)  ==>  " ^ bexp_to_string bexp3 ^ " \n");;
print_string ("       Non (Vrai)  ==>  " ^ bexp_to_string bexp4 ^ " \n");;
print_string ("       Ou(Vrai,Faux)  ==>  " ^ bexp_to_string bexp5 ^ " \n");;
print_string ("       Equal(Int 2, Int 4)  ==>  " ^ bexp_to_string bexp6 ^ " \n");;
print_string ("       Equal(Plus(Int 3, Int 5), Mult(Int 2, Int 4)) ==>  " ^ bexp_to_string bexp7 ^ " \n");;
print_string ("       Equal(Mult(Int 2, Var 'x'), Plus(Var 'y', Int 1))  ==>  " ^ bexp_to_string bexp8 ^ " \n");;
print_string ("       EqualOrInf(Int 5, Int 7)  ==>  " ^ bexp_to_string bexp9 ^ " \n");;
print_string ("       Et(EqualOrInf(Plus(Int 8, Int 9), Mult(Int 4, Int 5)),EqualOrInf(Plus(Int 3, Var 'x'), Mult(Int 4, Var 'y')))  ==>  " ^ bexp_to_string bexp10 ^ " \n");;

(* 1.2.2 Interprétation *)

(* Q4 : La fonction binterp qui renvoie (un booléen) à partir d'une expression et une valuation *)
let rec binterp exp valuation = match exp with 
Vrai -> true
| Faux -> false
| Et (argument1, argument2) ->  binterp argument1 valuation && binterp argument2 valuation
| Ou (argument1, argument2) ->   binterp argument1 valuation || binterp argument2 valuation
| Non  argument1 -> not (binterp argument1 valuation)
| Equal (argument1, argument2) ->   ainterp argument1 valuation == ainterp argument2 valuation
| EqualOrInf (argument1, argument2) ->  ainterp argument1 valuation <= ainterp argument2 valuation ;;  

(*Q5 : Evaluation via binterp sachant que x=7 et y=3*)
print_string("\n     Q5 : Evaluation via binterp sachant que x=7 et y=3  \n \n");;
let valuation2 =Val([("x", 7);("y", 3)]);;
let result1 = binterp bexp1 valuation2 ;;
print_string("       bexp1 ==> " ^string_of_bool(result1)^"\n");;
let result2 = binterp bexp2 valuation2 ;;
print_string("       bexp2 ==> " ^string_of_bool(result2)^"\n");;
let result3 = binterp bexp3 valuation2 ;;
print_string("       bexp3 ==> " ^string_of_bool(result3)^"\n");;
let result4 = binterp bexp4 valuation2 ;;
print_string("       bexp4 ==> " ^string_of_bool(result4)^"\n");;
let result5 = binterp bexp5 valuation2 ;;
print_string("       bexp5 ==> " ^string_of_bool(result5)^"\n");;
let result6 = binterp bexp6 valuation2 ;;
print_string("       bexp6 ==> " ^string_of_bool(result6)^"\n");;
let result7 = binterp bexp7 valuation2 ;;
print_string("       bexp7 ==> " ^string_of_bool(result7)^"\n");;
let result8 = binterp bexp8 valuation2 ;;
print_string("       bexp8 ==> " ^string_of_bool(result8)^"\n");;
let result9 = binterp bexp9 valuation2 ;;
print_string("       bexp9 ==> " ^string_of_bool(result9)^"\n");;
let result10 = binterp bexp10 valuation2 ;;
print_string("       bexp10 ==> " ^string_of_bool(result10)^"\n");;


(* 1.3  Les commandes du langage *)
print_string("\n \n 1.3 Les commandes du langage \n");;

(* 1.3.1 Syntaxe abstraite *)
print_string("\n   1.3.1 Syntaxe abstraite  \n");;

(* Q1 : Type prog définit la syntaxe abstraite des commandes du notre langage*)
type prog =
Skip
| Affect of string*aexp
| Sequence of prog*prog
| If of bexp*prog
| If_Else of bexp*prog*prog
| Repeat of aexp*prog ;;

(* Q2 : Ecriture des expressions*)
let prog1 = Affect ("y",Int 7);; 
let prog2 = Sequence ((Affect ("z",Plus(Int 3, Int 4))),((Affect ("x",Mult(Int 2, Var "x")))));;
let prog3 = Sequence ((Affect ("n",Int 3)),If_Else((EqualOrInf(Var "n",Int 4)),(Affect("n",Plus(Mult(Int 2,Var "n"),Int 3))),(Affect("n",Plus(Var "n",Int 1)))));;
let prog4 = Repeat (Int 10,Affect("x",Plus(Var "x",Int 1)));;

(* Q3-1 : Ecriture de la fonction prog_to_string qui transforme un programme en une chaîne de caractères*)  
let rec prog_to_string prog = match prog with 
| Skip -> "skip " 
| Affect (varName,expression) -> varName ^ " := " ^ aexp_to_string expression ^ "" 
| Sequence (instruction1,instruction2) -> prog_to_string instruction1 ^ " ; " ^ prog_to_string instruction2 ^ "\n" 
| If (condition,programme) -> "if (" ^ bexp_to_string condition ^ ") then " ^  prog_to_string programme ^ "\n" 
| If_Else (condition,programme,elseProgramme) -> "if (" ^ bexp_to_string condition ^ ") then " ^  prog_to_string programme ^ " else " ^ prog_to_string elseProgramme ^ " \n"
| Repeat (number,programme) -> "repeat " ^ aexp_to_string number ^ " do " ^ prog_to_string programme ^ " od \n";;

(*Q3-2 : Affichage des chaines de caractères de la question 2*)
print_string("\n     Q3-2 : Affichage des chaines de caractères correspondent aux programmes de la question 2 \n \n");;
print_string("       1:Affect ('y',Int 7)\n        ==> "^prog_to_string prog1^"\n");;
print_string("       2:Sequence ((Affect ('z',Plus(Int 3, Int 4))),((Affect ('x',Mult(Int 2, Var 'x')))))\n        ==> "^prog_to_string prog2);;
print_string("       3:Sequence ((Affect ('n',Int 3)),If_Else((EqualOrInf(Var 'n',Int 4)),(Affect('n',Plus(Mult(Int 2,Var 'n'),Int 3))),(Affect('n',Plus(Var 'n',Int 1)))))\n        ==> "^prog_to_string prog3);;
print_string("       4:Repeat (Int 10,Affect('x',Plus(Var 'x',Int 1)))\n        ==> "^prog_to_string prog4);;

(*  Q4 : Fonction selfcompose *)
let rec selfcompose n f x = if (n<=0) then x else  selfcompose (n-1) f (f x) ;;

(*Q5 : Application 10 fois de la fonction x=x+2 à la valeur 0 *)
print_string("\n     Q5 : Application 10 fois de la fonction x=x+2 à la valeur 0 \n ");;
let fct x = x+2 ;;
let result = selfcompose 10 fct 0 ;;
print_string("        selfcompose(10,x+2,0)  = " ^ string_of_int(result) ^ "\n" );;

(*  Q6  *)
(*a- Fonction pour changer la valeur de l'élément dans l'évaluation par une nouvelle valeur si elle existe sinon nous ajoutons cette variable à la liste  *)
let rec changeVarValue var newValue valuation = match valuation with
|Val ((name,value)::listValuation) ->  (if ((compare var name)== 0 ) then Val((name,newValue)::listValuation) 
                                        else ( let c=(changeVarValue var newValue (Val listValuation) ) in 
                                              ( match c with 
                                                |Val valuation -> Val ((name,value)::valuation))))
| Val ([]) -> Val([(var,newValue)]) ;;
(* b- Fonction qui convertit l'évaluation en chaîne *)
let rec valuation_to_string valuation = match valuation with 
| Val ((nom,value)::val')  -> nom ^ " = " ^ string_of_int(value) ^ "  " ^ valuation_to_string (Val val')
| Val([]) -> "\n";;

(* c- Nous avons créé cette fonction au lieu de la fonction selfcompose car nous ne pouvons pas exécuter la boucle de répétition à l'aide de la fonction selfcompose *)
let rec selfcomposeV2 n f prog valuation = if (n<=0) then valuation else let list=f prog valuation in selfcomposeV2 (n-1) f prog list ;; 

(* Q6:  Fonction exec qui prend en arguments un programme et une valuation qui contient les
valeurs initiales de toutes les variables du programme et qui renvoie une valuation qui contient
toutes les valeurs finales des variables du programme.*)
let rec exec prog valuation = match prog with 
| Skip -> valuation 
| Affect (varName,expression) -> changeVarValue varName (ainterp expression valuation) valuation 
| Sequence (instruction1,instruction2) -> exec instruction2 (exec instruction1 valuation)
| If (condition,programme) -> if(binterp condition valuation) then exec programme valuation else valuation
| If_Else (condition,programme,elseProgramme) -> if(binterp condition valuation) then exec programme valuation else exec elseProgramme valuation
| Repeat (number,programme) -> selfcomposeV2 (ainterp number valuation) exec programme valuation;;

(* Q7 :  *)
(* Q7-1 : Factorielle *)
let progFactorielle = If_Else(EqualOrInf(Var "n", Int 0), Affect("result", Int 1),Sequence(Affect("result", Int 1),Repeat(Var "n",Sequence(Affect("result", Mult(Var "result", Var "n")),Affect("n", Sub(Var "n",Int 1)) ))) );;
let varList=Val ([("n", 5)]);;
print_string("\n     Q7-1 :  Factorielle " ^ (valuation_to_string varList));;
let varListResult=exec progFactorielle varList ;;
print_string("       resultat = "^string_of_int(getVarValue "result" varListResult) ^ "\n");;


(*Q7-2:  Fibonacci *)
let progFibonacci = Sequence(Affect("a",Int 0), Sequence(Affect("b",Int 1) ,Repeat(Var "n", Sequence(Sequence(Affect("a'",Var "a") ,Affect("a",Var "b") ) , Affect("b",Plus(Var "a'", Var "b"))) ) ));;
let varList=Val ([("n", 8)]);;
print_string("\n     Q7-2 :  Fibonacci " ^ (valuation_to_string varList));;

let varListResult=exec progFibonacci varList ;;
print_string("       resultat = "^string_of_int(getVarValue "a" varListResult) ^ "\n");;


(* 1.4  Triplets de Hoare et validité  *)
print_string("\n \n 1.3  Triplets de Hoare et validité \n");;

(* 1.4.1 Syntaxe abstraite des formules de la logiques des propositions*)
print_string("\n   1.4.1 Syntaxe abstraite des formules de la logiques des propositions  \n");;

(*  4.1 formules de la logiques des propositions  *)
(*  Q1 : le type tprop donne la syntaxe abstraite des formules logiques. *)
type tprop =
Vraip
  | Fauxp
  | Etp of tprop*tprop
  | Oup of tprop*tprop
  | Nonp of tprop
  | Equalp of aexp*aexp
  | EqualOrInfp of aexp*aexp 
  | Impliquep of tprop*tprop  ;;

(*  Q2 : Ecriture des buts à l'aide de tprop *)

let tprop1 = Vraip;;
let tprop2 = Etp(Vraip,Fauxp);;
let tprop3 = Nonp Vraip;;
let tprop4 = Oup(Vraip,Fauxp);;
let tprop5 = Impliquep(Fauxp,Oup(Vraip,Fauxp)) ;;

let tprop6 =  Equalp(Int 2,Int 4);;
let tprop7 =  Equalp(Plus(Int 3, Int 5),Mult(Int 2, Int 4));;
let tprop8 =  Equalp(Mult(Int 2, Var "x"),Plus(Var "y", Int 1));;

let tprop9 = EqualOrInfp(Plus(Int 3, Var "x") ,Mult(Int 4, Var "y"));;
let tprop10 = Etp(EqualOrInfp(Int 5 ,Int 7 ),EqualOrInfp(Plus( Int 8, Int 9 ) ,Mult(Int 4, Int 5)));;
let tprop11 = Impliquep( Equalp(Var "x",Int 1),EqualOrInfp(Var "y",Int 0));;

(*  Q3  *)
(* Q3-1  Fonction bexp_to_string *) 
let rec prop_to_string prop  = match prop with
| Vraip -> "vrai"
| Fauxp -> "faux"
| Oup(argument1,argument2) -> "(" ^ prop_to_string argument1 ^ " ou " ^ prop_to_string argument2 ^ ")"
| Etp(argument1,argument2) -> "(" ^ prop_to_string argument1 ^ " et " ^ prop_to_string argument2 ^ ")"
| Nonp  argument1 -> "!(" ^ prop_to_string argument1 ^ ")" 
| Equalp (argument1, argument2) ->  "(" ^ aexp_to_string argument1 ^ " = " ^ aexp_to_string argument2 ^ ")"
| EqualOrInfp (argument1, argument2) ->  "(" ^ aexp_to_string argument1 ^ " <= " ^ aexp_to_string argument2 ^ ")" 
| Impliquep (argument1, argument2) -> "(" ^ prop_to_string argument1 ^ " implique " ^ prop_to_string argument2 ^ ")" ;;

(*Q3-2 : Affichage des chaines de caractères de la question 2*)
print_string("\n     Q3-2 : Affichage des chaines de caractères correspondent aux formules logiques de la question 2 \n \n");;
print_string ("       vrai  ==>  " ^ prop_to_string tprop1 ^ " \n");;
print_string ("       Et(Vrai,Faux)  ==>  " ^ prop_to_string tprop2 ^ " \n");;
print_string ("       Non Vrai  ==>  " ^ prop_to_string tprop3 ^ " \n");;
print_string ("       Ou(Vrai,Faux)  ==>  " ^ prop_to_string tprop4 ^ " \n");;
print_string ("       Implique(Faux,Ou(Vrai,Faux))  ==>  " ^ prop_to_string tprop5 ^ " \n");;
print_string ("       Equal(Int 2,Int4)  ==>  " ^ prop_to_string tprop6 ^ " \n");;
print_string ("       Equal(Plus(Int 3, Int 5),Mult(Int 2, Int 4))  ==>  " ^ prop_to_string tprop7 ^ " \n");;
print_string ("       Equal(Mult(Int 4, Var x),Plus(Var y, Int 1))  ==>  " ^ prop_to_string tprop8 ^ " \n");;
print_string ("       EqualOrInf(Plus(Int 3, Var x) ,Mult(Int 4, Var y))  ==>  " ^ prop_to_string tprop9 ^ " \n");;
print_string ("       Et(EqualOrInf(Int 5 ,Int 7 ),EqualOrInf(Plus( Int 8, Int 9 ) ,Mult(Int 4, Int 5)))  ==>  " ^ prop_to_string tprop10 ^ " \n");;
print_string ("       Implique( Equal(Var x,Int 1),EqualOrInf(Var y,Int 0))  ==>  " ^ prop_to_string tprop11 ^ " \n");;

(*  1.4.2 Interprétation  *)
print_string("\n   1.4.2 Interprétation  \n");;

(*  Q4 :La fonction pinterp qui prend en arguments une formule logique et une valuation et qui renvoie un booléen *)
let rec pinterp prop valuation = match prop with 
| Vraip -> true
| Fauxp -> false
| Oup(argument1,argument2) ->  pinterp argument1 valuation || pinterp argument2 valuation
| Etp(argument1,argument2) -> pinterp argument1 valuation && pinterp argument2 valuation
| Nonp  argument1 -> not (pinterp argument1 valuation) 
| Equalp (argument1, argument2) ->  ainterp argument1 valuation == ainterp argument2 valuation
| EqualOrInfp (argument1, argument2) ->  ainterp argument1 valuation <= ainterp argument2 valuation
| Impliquep (argument1, argument2) -> not( pinterp argument1 valuation) || ( pinterp argument2 valuation);;

(*  Q5 : Evaluation de expressions à l'aide de pinterp *)
print_string("\n     Q5 : Evaluation des expressions sachant que x=7 et y=3 \n \n");;
let valuation =Val([("x", 7);("y", 3)]);;
let result = pinterp tprop1 valuation ;;
print_string("       tprop1 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop2 valuation ;;
print_string("       tprop2 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop3 valuation ;;
print_string("       tprop3 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop4 valuation ;;
print_string("       tprop4 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop5 valuation ;;
print_string("       tprop5 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop6 valuation ;;
print_string("       tprop6 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop7 valuation ;;
print_string("       tprop7 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop8 valuation ;;
print_string("       tprop8 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop9 valuation ;;
print_string("       tprop9 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop10 valuation ;;
print_string("       tprop10 ==> " ^string_of_bool(result)^"\n");;
let result = pinterp tprop11 valuation ;;
print_string("       tprop11 ==> " ^string_of_bool(result)^"\n");;


(*  1.4.3 Substitutions  *)
print_string("\n   1.4.3 Substitutions  \n");;

(*  Q6 : La fonction psubst remplace une variable et une formule logique. *)
let rec psubst tprop var aexp =match tprop with 
| Vraip -> Vraip
| Fauxp -> Fauxp
| Oup(argument1,argument2) ->Oup((psubst argument1 var aexp ),(psubst argument2 var aexp ))
| Etp(argument1,argument2) -> Etp((psubst argument1 var aexp ),(psubst argument2 var aexp ))
| Nonp tprop' -> Nonp (psubst tprop' var aexp )
| Equalp(argument1,argument2) -> Equalp((asubst argument1 var aexp ),(asubst argument2 var aexp))
| EqualOrInfp(argument1,argument2) -> EqualOrInfp ((asubst argument1 var aexp ),(asubst argument2 var aexp))
| Impliquep(argument1,argument2) -> Impliquep((psubst argument1 var aexp),(psubst argument2 var aexp)) ;;


(* Q7 : Affichage des chaînes de caractères des formules logiques de la question 2 pour lesquelles les variables x et y sont 
respectivement substituées par les expressions 3 ∗ y et k + 2.*)
print_string("\n     Q5 : Affichage des  des formules logiques sachant que x=3*y et y=k*2 \n \n");;
let aexpX=Mult(Int 3, Var "y");;
let aexpY=Plus(Var "k", Int 2);;

let resultaexp = psubst tprop8 "x" aexpX ;;
let resultaexp = psubst resultaexp "y" aexpY ;;
print_string("       formule logique 8 = "^prop_to_string tprop8 ^ " ==>  " ^ prop_to_string resultaexp ^ "\n");;
let resultaexp = psubst tprop9 "x" aexpX ;;
let resultaexp = psubst resultaexp "y" aexpY ;;
print_string("       formule logique 9 = "^prop_to_string tprop9 ^ " ==>  " ^ prop_to_string resultaexp ^ "\n");;
let resultaexp = psubst tprop11 "x" aexpX ;;
let resultaexp = psubst resultaexp "y" aexpY ;;
print_string("       formule logique 11 = "^prop_to_string tprop11 ^ " ==>  " ^ prop_to_string resultaexp ^ "\n");;


(*  1.4.4 Triplet de Hoare  *)
print_string("\n   1.4.4 Triplet de Hoare  \n");;

(*  Q8 : le type hoare_triple représente un triplet de Hoare *)
type hoare_triple =
Triplet of tprop*prog*tprop;;

(*  Q9 : Ecriture des triplets à l'aide de hoare_triplet *)
let hoare_triple1 = Triplet( Equalp(Var "x", Int 2), Skip, Equalp( Var "x", Int 2));;
let hoare_triple2 = Triplet( Equalp(Var "x", Int 2), Affect("x", Int 3 ), EqualOrInfp(Var "x", Int 3));;
let hoare_triple3 = Triplet( Vraip, If_Else(EqualOrInf(Var "x", Int 0),  Affect("r", Sub(Int 0, Var "x")),Affect("r", Var "x")),EqualOrInfp(Int 0, Var "r"));;
let hoare_triple4 = Triplet( Etp(Equalp(Var "in",Int 5),Equalp(Var "out", Int 1)),progFactorielle,Etp(Equalp(Var "in",Int 0),Equalp(Var "out", Int 120)));;

print_string("\n   1.4.5 Validité d’un triplet de Hoare \n");;

(*  Q10 : la fonction htvalid_test renvoie un booléen qui indique si un triplet est valide.*)
let rec htvalid_test h_triple valuation =match  h_triple with 
| Triplet(prec,prog,postc) -> (pinterp prec valuation) && (pinterp postc (exec prog valuation ));;

print_string("\n     Q5 : Vérification de validité  \n");;

let valuation=Val ([("x", 2);("y", 3)]);;
print_string("       Expression1: Expected True --- result ==> " ^string_of_bool(htvalid_test hoare_triple1 valuation )^"\n");;
let valuation=Val ([("x", 2)]);;
print_string("       Expression2: Expected True --- result ==> " ^string_of_bool(htvalid_test hoare_triple2 valuation )^"\n");;
let valuation=Val ([("x", -2)]);;
print_string("       Expression3: Expected True --- result ==> " ^string_of_bool(htvalid_test hoare_triple3 valuation )^"\n");;



(*Partie 2 :  Un (mini) prouveur en logique de Hoare*)
print_string("\n\n\n----------------------------   Partie 2 :  Un (mini) prouveur en logique de Hoare   ----------------------------\n");;

(* 2.1 Les buts de preuves et le langage des tactiques *)
print_string("\n  2.1 Les buts de preuves et le langage des tactiques \n");;

(* 1.1.1 Syntaxe abstraite *)
print_string("\n   2.1.1 Les buts de preuves \n");;

(*  Les buts de preuves et le langage des tactiques *)
(*  Q1:  *)
type context=
Context of (string*tprop) list;;

type conclusion =
 ConclusionProp of tprop 
| ConclusionHoare of hoare_triple;;

type goal =
|Goal of  context*conclusion;;

(*  Q2:  *)
let p :tprop = Vraip;;
let q :tprop = Fauxp;;
let r :tprop = Vraip;;

let goal1 = Goal(Context([("H",Impliquep(Oup(p,q),r));("H2",p)]),ConclusionProp(Oup(p,q)));;
let goal2 = Goal(Context [], ConclusionHoare(Triplet( Equalp(Var "x", Int (-3)), If_Else(EqualOrInf(Var "x", Int 0),  Affect("x", Sub(Int 0, Var "x")),Affect("x", Var "x")),Equalp(Var "x", Int 3))));;


(*  Q3:   *)

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

print_string("\n     Q3 : Affichage des buts  \n");;
print_string("\n------1er Goal:  \n\n");;
print_string(print_goal goal1);;
print_string("\n------2eme Goal:  \n\n");;
print_string(print_goal goal2);;


let fresh_ident =
      let prefix = "H" and count = ref 0
      in
      function () -> ( count := ! count + 1 ;
      prefix ^ ( string_of_int (! count )))
  ;;



(* Q4 : L'arbre de preuve
   L'arbre qui montre la suite des règles de déduction à appliquer pour prouver le triplet.

{(x = y + i - 1) /\ (i <= 10)} c {[i + 1/i](x = y + i - 1)}
------------------------------------------------------------ repeat(i)
{[1/i](x = y + i - 1)} repeat 10 do c {(x = y + i - 1) /\ (i = 10 + 1)}

*)

(* Q5: La preuve du triplet

{(r = 0) /\ (n = 1)} repeat 5 do r := r + n; n := n + 1 od {(r = 15) /\ (n = 6)}
I = (r = i * (i-1) / 2) /\ (n = i)
{(r = 0) /\ (n = 1)}
{I}
repeat 5 do 
   {(r = i * (i-1) / 2) /\ (n = i) /\ i <= 5}
   r := r + n; 
   {(r + n = i * (i-1) / 2) /\ (n = i) /\ i <= 5}
   n := n + 1 
   {(r + n + 1 = i * (i-1) / 2) /\ (n + 1 = i) /\ i <= 5}
od
{(r = i * (i-1) / 2) /\ (n = i) /\ i = 5 + 1}
{(r = 15) /\ (n = 6)}
*)


(*  Q6 :  *)
type tactic =
  And_Intro
  | Or_Intro_1
  | Or_Intro_2
  | Impl_Intro
  | Not_Intro
  | And_Elim_1 of string
  | And_Elim_2 of string 
  | Or_Elim of string 
  | Impl_Elim of string*string
  | Not_Elim of string*string
  | Exact of string 
  | Assume of tprop
  | Admit
  | HSkip 
  | HAssign
  | HIf
  | HRepeat of string 
  | HCons of tprop*tprop
  | HSEq of tprop*tprop;;

print_string("\n  2.2 Appliquer une tactique à un but \n");;
 
(* Q1: *)

let rec bool2prop bexp = match bexp with 
Vrai -> Vraip
| Faux -> Fauxp
| Et (left_argument, right_argument) ->  Etp ((bool2prop left_argument), (bool2prop right_argument))
| Ou (left_argument, right_argument) ->    Oup ((bool2prop left_argument), (bool2prop right_argument))
| Non  exp -> Nonp (bool2prop exp)
| Equal (left_argument, right_argument) ->   Equalp (left_argument, right_argument)
| EqualOrInf (left_argument, right_argument) -> EqualOrInfp (left_argument, right_argument) ;;  


 
(* Q2: *)

let add_prop_to_context prop context = match context with
|Context ([]) -> Context( [( fresh_ident () , prop )] )
|Context (listContext) -> Context (listContext@[( fresh_ident () , prop )]) ;;

let rec get_prop_from_context name context = match context with 
|Context ([]) -> failwith "name not found in Hypothesis"
|Context ((name',prop)::listContext) -> if ((compare name' name)== 0 ) then prop else get_prop_from_context name (Context listContext)  ;;

let rec replace_prop_in_context old_prop_name new_prop context = match context with
|Context ([]) -> failwith "name not found in Hypothesis"
|Context ((name,prop)::listContext) ->  if ((compare old_prop_name name)== 0 ) then Context((name,new_prop)::listContext) 
                                        else ( let c=(replace_prop_in_context old_prop_name new_prop (Context listContext) ) in 
                                        ( match c with 
                                          |Context context -> Context ((name,prop)::context)
 ) );;


let apply_prop_tactic context prop tactic = match tactic with
|And_Intro ->( match prop with 
              | Etp(prop1,prop2) -> [(Goal(context,ConclusionProp(prop1)));(Goal(context,ConclusionProp(prop2)))]
              | _ -> failwith "Goal is not an And-formula" )
|Or_Intro_1 -> ( match prop with 
              | Oup(prop1,prop2) -> [(Goal(context,ConclusionProp(prop1)))]
              | _ -> failwith "Goal is not an Or-formula" )
|Or_Intro_2 -> ( match prop with 
              | Oup(prop1,prop2) -> [(Goal(context,ConclusionProp(prop2)))]
              | _ -> failwith "Goal is not an Or-formula" )
|Impl_Intro -> ( match prop with 
              | Impliquep(prop1,prop2) -> [Goal(add_prop_to_context prop1 context ,ConclusionProp(prop2)  )]
              | _ -> failwith "Goal is not an Implication-formula" )
        
|Not_Intro -> ( match prop with 
              | Nonp prop1 -> [Goal(add_prop_to_context prop1 context ,ConclusionProp(Fauxp)  )]
              | _ -> failwith "Goal is not a Not-formula" )
|And_Elim_1 name -> ( match (get_prop_from_context name context) with 
                     | Etp (prop1,prop2) -> [Goal(add_prop_to_context prop1 context ,ConclusionProp(prop)  )]
                     | _ -> failwith "Hypothesis is not an And-formula" ) 
|And_Elim_2 name -> ( match (get_prop_from_context name context) with 
                     | Etp (prop1,prop2) -> [Goal(add_prop_to_context prop2 context ,ConclusionProp(prop)  )]
                     | _ -> failwith "Hypothesis is not an And-formula" )           
|Or_Elim name -> ( match (get_prop_from_context name context) with 
                     | Oup (prop1,prop2) ->  [(Goal((replace_prop_in_context name prop1 context),ConclusionProp(prop)));(Goal((replace_prop_in_context name prop2 context),ConclusionProp(prop)))]
                     | _ -> failwith "Hypothesis is not an Or-formula" )       
|Impl_Elim (name1,name2) ->( match  (get_prop_from_context name1 context) with
                     |Impliquep(leftPart,rightPart) -> if ((leftPart)=(get_prop_from_context name2 context)) then [Goal(add_prop_to_context rightPart context ,ConclusionProp(prop)  )]
                                                                                                             else failwith "Second hypothesis does not match the assumption of the first hypothesis"
                     | _ -> failwith "First hypothesis is not an Implication-formula" )  
|Not_Elim (name1,name2) ->( match  (get_prop_from_context name1 context) with
                     |Nonp(prop1) -> if ((prop1)=(get_prop_from_context name2 context)) then [Goal(add_prop_to_context Fauxp context ,ConclusionProp(prop)  )]
                                                                                                             else failwith "Second hypothesis does not match the assumption of the first hypothesis"
                     | _ -> failwith "First hypothesis is not an Not-formula" )  
|Exact name -> if((get_prop_from_context name context)=prop) then [] else failwith "can not apply the goal"
|Assume prop1 -> [Goal(add_prop_to_context prop1 context ,ConclusionProp(prop));Goal(context ,ConclusionProp(prop1))];;


let apply_hoare_tactic context prop tactic = match tactic with
|HSkip -> [Goal(context,ConclusionHoare(prop))]
| _ -> [Goal(context,ConclusionHoare(prop))] ;;


let rec apply_tactic goals tactic= match goals with 
|Goal(context,conclusion)::goalList' ->(match conclusion with 
                                       | ConclusionProp prop ->  (apply_prop_tactic context prop  tactic) @ goalList'
                                       | ConclusionHoare hoare_triple -> (apply_hoare_tactic context hoare_triple tactic) @ goalList' );;
  
(* Q3 :*)

let p :tprop = Vraip;;
let q :tprop = Fauxp;;
let r :tprop = Vraip;;



let rec print_goals goals gloalnumber = match  goals with 
| [] -> " no more subgoals \n"
| [Goal(context,conclusion)] -> "subgoals "^ (string_of_int  gloalnumber) ^ " \n"  ^ print_goal (Goal(context,conclusion)) ^ " \n"
| Goal(context,conclusion)::goals' -> "subgoals "^ (string_of_int  gloalnumber) ^ " \n" ^ print_goal (Goal(context,conclusion)) ^ " \n" ^ (print_goals goals' (gloalnumber+1) ) ;;


let formule=Impliquep(Impliquep(Oup( p,q ) ,r ) ,Etp(Impliquep(p ,r ) ,Impliquep(q,r) ));;
let goalr=[Goal(Context([]),ConclusionProp(formule))];;

print_string("\n  Utilisation de la  fonction apply_tactic pour prouver le but suivant (P ∨ Q ⇒ R) ⇒ (P ⇒ R) ∧ (Q ⇒ R) \n");;

print_string("\n\n");;

print_string(print_goals goalr 1);;

let goalr = apply_tactic goalr Impl_Intro;;
print_string("\n---------------------------Operation : Impl_Intro\n");;
print_string(print_goals goalr 1);;
print_string("\n");;

let goalr = apply_tactic goalr And_Intro;;
print_string("\n---------------------------Operation : And_Intro\n");;
print_string(print_goals goalr 1);;
print_string("\n");;

let goalr = apply_tactic goalr Impl_Intro;;
print_string("\n---------------------------Operation : Impl_Intro\n");;
print_string(print_goals goalr 1);;
print_string("\n");;

let goalr = apply_tactic goalr (Assume (Oup(p,q)));;
print_string("\n---------------------------Operation : Assume (p\/q) \n");;
print_string(print_goals goalr 1);;
print_string("\n");;

let goalr = apply_tactic goalr (Impl_Elim ("H1","H3") );;
print_string("\n---------------------------Operation : Impl_Elim ('H1','H3') \n");;
print_string(print_goals goalr 1);;
print_string("\n");;

let goalr = apply_tactic goalr (Exact "H4");;
print_string("\n---------------------------Operation : Exact 'H4' \n");;
print_string(print_goals goalr 1);;
print_string("\n");;
 
let goalr = apply_tactic goalr (Or_Intro_1);;
print_string("\n---------------------------Operation : Or_Intro_1 \n");;
print_string(print_goals goalr 1);;
print_string("\n");; 

let goalr = apply_tactic goalr (Exact "H2");;
print_string("\n---------------------------Operation : Exact 'H2' \n");;
print_string(print_goals goalr 1);;
print_string("\n");;

let goalr = apply_tactic goalr (Impl_Intro);;
print_string("\n---------------------------Operation :Impl_Intro \n");;
print_string(print_goals goalr 1);;
print_string("\n");;

let goalr = apply_tactic goalr (Assume (Oup(p,q)));;
print_string("\n---------------------------Operation : Assume (p\/q) \n");;
print_string(print_goals goalr 1);;
print_string("\n");;

let goalr = apply_tactic goalr (Impl_Elim ("H1","H6") );;
print_string("\n---------------------------Operation : Impl_Elim ('H1','H6') \n");;
print_string(print_goals goalr 1);;
print_string("\n");;

let goalr = apply_tactic goalr (Exact "H7");;
print_string("\n---------------------------Operation : Exact 'H7' \n");;
print_string(print_goals goalr 1);;
print_string("\n");;

let goalr = apply_tactic goalr (Or_Intro_2);;
print_string("\n---------------------------Operation : Or_Intro_2 \n");;
print_string(print_goals goalr 1);;
print_string("\n");; 

let goalr = apply_tactic goalr (Exact "H5");;
print_string("\n---------------------------Operation : Exact 'H5' \n");;
print_string(print_goals goalr 1);;
print_string("\n");;


(*Q4 : Utilisation de la fonction apply_tactic pour prouver les triplets de Hoare suivants : *)
let hoare_triple1 = Triplet( Equalp(Var "x", Int 2), Skip, Equalp( Var "x", Int 2));;
let hoare_triple2 = Triplet( EqualOrInfp(Plus(Var "y", Int 1),Int 4),Affect("y",Plus(Var "y", Int 1)), EqualOrInfp(Var "y", Int 4));;
let hoare_triple3 = Triplet( Equalp(Var "y", Int 5), Affect("x", Plus(Var "y", Int 1)), Equalp( Var "x", Int 6));;
let hoare_triple4 = Triplet( Vraip, Sequence(Sequence (Affect("z", Var "x"),Affect("z", Plus(Var "z", Var "y"))),Affect("u", Var "z")), Equalp( Var "u", Plus(Var "x", Var "y")));;
let hoare_triple5 = Triplet( Vraip, If_Else(EqualOrInf(Var "v", Int 0),  Affect("r", Sub(Int 0, Var "v")),Affect("r", Var "v")) , EqualOrInfp( Int 0, Var "r"));;
let hoare_triple6 = Triplet( Equalp(Var "x", Var "y"), Repeat(Int 10,Affect("x", Plus(Var "x", Int 1))), Equalp( Var "x", Plus(Var "y", Int 10)));;


(*Q5 : Vérification de la validité  *)
let valuation =Val([("x", 2)]);;
print_string("       Test Skip:      Expected True ---  result ==> " ^string_of_bool(htvalid_test hoare_triple1 valuation )^"\n");;

let valuation =Val([("y", 0)]);;
print_string("       Test Affect:    Expected True ---  result ==> " ^string_of_bool(htvalid_test hoare_triple2 valuation )^"\n");;

let valuation =Val([("y", 5)]);;
print_string("       Test Affect:    Expected True ---  result ==> " ^string_of_bool(htvalid_test hoare_triple3 valuation )^"\n");;

let valuation =Val([("y", 7)]);;
print_string("       Test Affect:    Expected False --- result ==> " ^string_of_bool(htvalid_test hoare_triple3 valuation )^"\n");;

let valuation =Val([("x", 5);("y",1)]);;
print_string("       Test Sequence:  Expected True ---  result ==> " ^string_of_bool(htvalid_test hoare_triple4 valuation )^"\n");;

let valuation =Val([("v", -5)]);;
print_string("       Test If:        Expected True ---  result ==> " ^string_of_bool(htvalid_test hoare_triple5 valuation )^"\n");;

let valuation =Val([("v", 5)]);;
print_string("       Test If:        Expected True ---  result ==> " ^string_of_bool(htvalid_test hoare_triple5 valuation )^"\n");;

let valuation =Val([("x", 10);("y", 10)]);;
print_string("       Test Repeat:    Expected True ---  result ==> " ^string_of_bool(htvalid_test hoare_triple6 valuation )^"\n");;
