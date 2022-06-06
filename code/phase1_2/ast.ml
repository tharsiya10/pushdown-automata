type automate= 
| Automate of declaration * transitions

and declaration = 
|Declaration of inputsymbols * stacksymbols * states * initstate * initstack

and lettre = 
| Lettre of string 

and inputsymbols = 
|Inputsymbols of lettre list

and stacksymbols = 
|Stacksymbols of lettre list 

and states = 
|States of lettre list

and initstate = 
| InitState of lettre 

and initstack = 
| InitStack of lettre 

and suitedelettrenonvide = 
| SuitelettreNonVide of lettre list  

and transitions = 
|Transitions of translist

and translist = 
|Translist of transition list

and transition = 
| Transition of lettre * lettreouvide * lettre * lettre * stack 

and lettreouvide = 
|Lettreouvide of string 
|Epsilon 



and stack =  
|Stack of lettre list 


let rec as_string_automate = function
  | Automate (a,b) -> as_string_declaration a ^ as_string_transitions b 

  and as_string_declaration x = match x with 
  |Declaration (a,b,c,d,e) ->  
    as_string_inputsymb a ^ as_string_stacksymb b ^
    as_string_states c ^ as_string_initstate d ^
    as_string_initstack e

  and as_string_inputsymb sl = match sl with 
  |Inputsymbols l ->  "input symbols: " ^ as_string_suitelettre l ^"\n"

  and as_string_stacksymb sl = match sl with 
  |Stacksymbols l  ->  "stack symbols: " ^ as_string_suitelettre l ^"\n"

  and as_string_states sl = match sl with 
  |States l ->  "stack: " ^ as_string_suitelettre l ^"\n"
  
  and as_string_initstate x  = match x with 
  | InitState y -> "initial state: "^ as_string_lettre y ^"\n"

  and as_string_initstack x  = match x with 
  | InitStack y -> "initial stack symbols: "^  as_string_lettre y  ^"\n\n"

  and as_string_lettre a = match a with
  | Lettre y -> y  

  and as_string_suitelettre sl = match sl with 
  |[] -> ""
  |Lettre(x)::[]-> x
  |Lettre(x)::r ->  x ^ ", " ^ as_string_suitelettre r

  and as_string_transitions tlist = match tlist with 
  |Transitions tl -> "transitions: \n\n" ^ as_string_translist tl  

  and as_string_translist l = match l with 
  |Translist(trl)->  as_string_trlist trl 

  and as_string_trlist trlist = match trlist with 
  |[]->""
  |Transition(a,b,c,d,e) ::rest -> 
    "(" ^ (as_string_lettre a) ^ "," ^ (as_string_lettreouvide b) ^ "," ^
    (as_string_lettre c) ^ "," ^ (as_string_lettre d) ^ "," ^
    (as_string_stack e) ^") "^"\n" ^(as_string_trlist rest) 


  and as_string_stack a = match a with 
  | Stack (x) -> as_string_suitelettreouvide x 

  and as_string_suitelettreouvide sl = match sl with 
  |[]-> ""
  |Lettre(x)::[]-> x
  |Lettre(x)::r ->  x ^ ";" ^ as_string_suitelettre r



  and as_string_lettreouvide a = match a with
  | Epsilon -> ""
  | Lettreouvide y -> y 


