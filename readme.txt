
Projet Logiciels sûrs : Un interpréteur pour un langage impératif et un prouveur en logique de Hoare
—Master Informatique, 1ière année, 2ème semestre, 2021-2022— 

Réalisé par : Aymen OUADEN - Islem CHEKLAT - Abel AMOUH

Language : OCAML

Remarques:
*** Il faut lancer la commande Ocaml projetls.ml pour exécuter le programme.

Type valuation: c'est simplement une liste de (char * int). 
Fonctions associés :
                    - getVarValue : Prend un nom en char,un int et une valuation, compare chaque élément de valuation avec le nom envoyé et s'il match, renvoyer le int associer à cet element de valuation.
                    - changeVarValue : Prend un nom en char, un int (newValue) et une valuation, compare chaque element de valuation avec le nom envoyé et s'il match, on change le int de cet élément de valuation avec le int (newValue) sinon crée un nouveau élément (nom,newValue) et on ajoute à valuation.

selfcomposeV2 n f prog valuation : Nous avons créé cette fonction au lieu de la fonction selfcompose car nous ne pouvons pas exécuter la boucle de répétition à l'aide de la fonction selfcompose. 

Type hoare_triple: Composé d'un tprop et un prog et un tprop (proposition, programme, proposition)

type context : une liste de string * tprop.

type conclusion = Il y a deux types de conclusion, soit une proposition soit un triplet d'hoare.

type goal = il se compose d'un contexte et une conclusion.

apply_prop_tactic : Fonction pour appliquer une tactic sur la partie logique des propositions.

apply_hoare_tactic : Fonction pour appliquer une tactic sur la partie logique de Hoare ( Ne foncionne pas ). 

apply_tactic : cette fonction prend une liste des buts et applique une tactic sur le premier but de cette liste, dans le premier apelle elle prend
               une liste qui contient un seule but, après chaque apelle de la fonction, des buts sont supprimés ou bien ajoutés à la liste en fonction de tactic appliquée. 

prints_goals : Fonction qui convertit une liste des buts en chaîne.
