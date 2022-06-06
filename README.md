Compilation : 
- dans un terminal, lancer la commande "make" dans le répertoire code/phase1_2/ et code/phase3/
 
Execution
pour afficher le lexer :
./parser -print <chemin/vers/fichier>
pour executer l'automate :
./parser -exec <chemin/vers/fichier> <mot>

Pour chaque phase, il y a des fichiers contenant un automate dans les répertoires gasp2/exemples_phaseX/

(Phase 1)
Nous avons réalisé le lexer, le parser, l'arbre syntaxique et l'interpréteur. 
D'autre part, notre programme traite proprement les erreurs qui peuvent se produire pendant l'interprétation tel que : 
 	- il n'y a aucune transition qui s'applique 
	- l'entrée est épuisée sans que la pile soit vide 
	- la pile est vide sans que l'entrÃ©e soit épuisée.

(Phase 2)
Notre programme vérifie aussi la bonne formation de l'automate :
	- l'état initial est un élément de l'ensemble des états 
	- le symbole de pile initial est dans l'ensemble des symboles de pile ;
	- l'automate est déterministe 

(Phase 3)
Nous avons ensuite implémenter une syntaxe des automates à pile
ainsi que son interpréteur
Nous avons décidé de représenter epsilon par le caractère "_"
De même, il est possible d'indiquer plusieurs opérations séparés d'un point virgule. 
Par exemple,
"case next of 
a: push X; push X
etc"

VIGNESWARAN Tharsiya : 
une partie de l'ast.ml et du parser.mly de la phase 1. 
Réalisation de l'interpréteur pour la phase 1 et phase 3


TRAN Marilyn : 
en charge de ast.ml, lexer.mll et parser.mly de la phase 1 et de la phase 3 (avec l'aide de Tharsiya également).
Réalisation de la nouvelle grammaire de la phase 3 avec Tharsiya. 


