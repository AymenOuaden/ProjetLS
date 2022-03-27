

type valuation: c'est simplement une liste de (char * int)
Fonctions associés :
  - getVarValue : Prend un nom en char,un int et une valuation, compare chaque élément de valuation avec le nom envoyé et s'il match, renvoyer le int associer à cet           element de valuation.
  - changeVarValue : Prend un nom en char, un int (newValue) et une valuation, compare chaque element de valuation avec le nom envoyé et s'il match, on change le int de       cet élément de valuation avec le int (newValue) sinon crée un nouveau élément (nom,newValue) et on ajoute à valuation.


