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
%token EOF
(*Keywords*)
%token INPUT_SYMB STACK_SYMB STATES INIT_STATE INIT_STACK  
        PROGRAM POP PUSH REJECT CHANGE 
        END BEGIN
(*Literals*)
%token <string> LETTRE CASE_NEXT CASE_TOP CASE_STATE EPSILON

%start<Ast.automate> input

%%

input: c=automate EOF { c }

lettre: 
x = LETTRE {Lettre x}

automate:
x = declaration y=program {Automate (x,y)}

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

program:
|PROGRAM COLON c = cases {Program (c)}

instruction:
|POP {Pop}
|PUSH x=lettre  {Push x}
|CHANGE x=lettre  {Change x}
|REJECT {Reject}


condition:
| l = lettre COLON i = separated_nonempty_list(SEMICOLON,instruction) {Condition(l,i)} 
| l = lettre COLON BEGIN c=cases END {CondCases(l,c)}

casename:
| CASE_NEXT {Next}
| CASE_STATE {State}
| CASE_TOP {Top}

cases:
| c = casename lc = list(condition) {Cases(c,lc)}
