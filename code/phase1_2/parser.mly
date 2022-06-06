%{
open Ast
%}
/*
en capital les non-terminaux
en minuscule les terminaux
*/
(*Punctuation*)
%token COMMA COLON SEMICOLON
(*Structural symbols*)
%token LPAREN RPAREN EOF
(*Keywords*)
%token INPUT_SYMB STACK_SYMB STATES INIT_STATE INIT_STACK TRANSITIONS 
(*Literals*)
%token <string> LETTRE EPSILON

%start<Ast.automate> input

%%


  
input: c=automate EOF { c }

lettre: 
x = LETTRE {Lettre x}

automate:
x = declaration y=transitions {Automate (x,y)}

declaration:
a=inputsymbols b=stacksymbols c=states d=initstate e=initstack {Declaration(a,b,c,d,e)}

inputsymbols: 
INPUT_SYMB COLON x=separated_nonempty_list(COMMA,lettre)  {Inputsymbols(x)}

stacksymbols:
STACK_SYMB COLON  x=separated_nonempty_list(COMMA,lettre) {Stacksymbols(x)}

states: 
STATES COLON x=separated_nonempty_list(COMMA,lettre) {States(x)}

initstate:
INIT_STATE COLON x=lettre {InitState (x)}

initstack: 
INIT_STACK COLON x=lettre {InitStack x}

transitions:
TRANSITIONS COLON tl=translist {Transitions(tl)}

translist:
|x =list(transition) {Translist (x)}

transition:
LPAREN a=lettre COMMA b=lettreouvide COMMA c=lettre COMMA d=lettre COMMA e=stack RPAREN {Transition (a,b,c,d,e)}

lettreouvide:
|x=LETTRE {Lettreouvide x}
|{Epsilon}


stack:
|x =separated_list(SEMICOLON,lettre) {Stack(x)}


