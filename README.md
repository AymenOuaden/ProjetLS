
Projet Logiciels sûrs : Un interpréteur pour un langage impératif et un prouveur en logique de Hoare
—Master Informatique, 1ière année, 2ème semestre, 2021-2022— 

Réalisé par : Islem CHEKLAT - Aymen OUADEN - Abel AMOUH

Language : OCAML


Question 4 : L'arbre de preuve -----------------------------------------------------

{(x = y + i - 1) /\ (i <= 10)} c {[i + 1/i](x = y + i - 1)}
------------------------------------------------------------ repeat(i)
{[1/i](x = y + i - 1)} repeat 10 do c {(x = y + i - 1) /\ (i = 10 + 1)}
-------------------------------------------------------------------------------------


Question 5 :  La preuve du triplet --------------------------------------------------
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
---------------------------------------------------------------------------------------

Remarques:
*** Il faut lancer la commande Ocaml projetls.ml pour exécuter le programme.
*** Tout le code est commenté.
*** Voici quelques détails supplémentaires concernant le codage du projet: 

Type valuation: c'est simplement une liste de (char * int). 
Fonctions associés :
                    - getVarValue : Prend un nom en char,un int et une valuation, compare chaque élément de valuation avec le nom envoyé et s'il match, renvoyer le int associer à cet element de valuation.
                    - changeVarValue : Prend un nom en char, un int (newValue) et une valuation, compare chaque element de valuation avec le nom envoyé et s'il match, on change le int de cet élément de valuation avec le int (newValue) sinon crée un nouveau élément (nom,newValue) et on ajoute à valuation.

selfcomposeV2 n f prog valuation : Nous avons créé cette fonction au lieu de la fonction selfcompose car nous ne pouvons pas exécuter la boucle de répétition à l'aide de la fonction selfcompose. 

Type hoare_triple: Composé d'un tprop et un prog et un tprop (proposition, programme, proposition)

type context : une liste de string * tprop.

type conclusion = Il y a deux types de conclusion, soit une proposition soit un triplet d'hoare.

type goal = il se compose d'un contexte et une conclusion.

tactic : 

apply_prop_tactic : 

apply_hoare_tactic : 

prints_goals : 
