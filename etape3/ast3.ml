(*
Etape 3 AST : 
  type lettre = 
    |Lettre
  
  type instruction = 
    | push
    | change
    | pop
    | reject
  
  type cas = 
    | (instruction * lettre) list
    | next of (lettre * cas) list
    | top of (lettre * cas) list
    | state of (lettre * cas) list
  
  type program = 
    | cas list
*)