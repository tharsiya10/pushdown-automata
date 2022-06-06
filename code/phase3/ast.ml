type automate= 
| Automate of declaration * program

and declaration = 
|Declaration of inputsymbols * stacksymbols * states * initstate * initstack

and lettre = 
|Lettre of string 

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


and lettreouvide = 
|Lettreouvide of string 
|Epsilon 


and program =
|Program of cases

and cases =
|Cases of casename * (condition list)

and casename = 
|Next
|State
|Top


and condition = 
|Condition of lettre * (instruction list)
|CondCases of lettre * cases 


and instruction = 
|Pop  
|Reject 
|Push of lettre 
|Change of lettre   




let rec as_string_automate = function
  | Automate (a,b) -> as_string_declaration a ^ as_string_program b 

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


  and as_string_program p = match p with 
  |Program cs -> "program : \n\n" ^as_string_cases cs 1

  and as_string_cases c count = match c with 
  |Cases (c,lc) ->  (as_string_tab count) ^ as_string_casename c ^as_string_cond lc (count+1)

  and as_string_casename c = match c with 
  |Next-> "case next of \n"
  |Top-> "case top of \n"
  |State->  "case state of \n"

  
  and as_string_instruction ins = match ins with
  |[] -> "\n"
  |Pop :: t -> " pop " ^ as_string_instruction t
  |Reject :: t -> " reject "^as_string_instruction t
  |Push l ::t -> " push " ^as_string_lettre l^" "^as_string_instruction t
  |Change l :: t-> " change " ^as_string_lettre l ^" "^as_string_instruction t

  and as_string_cond c count = match c with 
  |[] -> ""
  |Condition (l,ins) :: rest -> (as_string_tab count)^as_string_lettre l ^ " : " ^ as_string_instruction ins ^as_string_cond rest count
  |CondCases (l,c) :: rest -> (as_string_tab count)^as_string_lettre l ^ " : begin \n" ^ as_string_cases c (count+1)^ (as_string_tab (count+1))^ "end\n" ^ as_string_cond rest (count)

  and as_string_tab count = 
    match count with
    | 0 -> ""
    | n -> "    "^as_string_tab (count-1)
