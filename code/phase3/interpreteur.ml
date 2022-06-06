open Ast
open String
open Stack
open Array

(*Variables globales stockant le mot, la pile et l'état courant au fur et à mesure de l'execution*)

let curr_mot = ref("") ;;
let curr_state = ref("") ;;
let curr_pile = ref(Stack.create());;
let cases = ref(Cases(Next,[]));;
let all_stacksymbols = ref(Stacksymbols([]));;
let all_states = ref(States([]));;

(* affiche contenu de la pile top from bottom *)
let print_stack print_elem stack = Stack.iter print_elem stack

let print_lettre l = 
  match l with
  | Lettre l -> print_string(l)


let get_init_state s = 
  match s with
  | InitState(Lettre(l)) -> l

let get_init_stack s = 
  match s with
  | InitStack(ll) -> ll

let get_cases p =
  match p with
  | Program(c) -> c
;;

 (* recuperer les donnees de l'automate donne *)                  
let get_automate a = 
  match a with
  | Automate (decl,progr) -> 
     (match decl,progr with
      | Declaration(_,stacksymb,states,state,stack),progr -> 
        all_stacksymbols := stacksymb;
        all_states := states;
        curr_state := get_init_state state ; 
        (push (get_init_stack stack) !curr_pile ) ;
        cases:= get_cases(progr) ;
     ) 

let update_value var value = 
  var := value

let empile a pile = 
  Stack.push a pile

let depile pile = 
  if(Stack.is_empty pile) then None else Some (Stack.pop pile)
  
let get_top pile = 
  if(Stack.is_empty pile) then None else Some(Stack.top pile)

let get_top_unit pile =
  let opt = get_top pile in
  (match opt with
   | _ -> ()
  )
  
let depile_unit pile =
  let opt = depile pile in
  (match opt with
   | _ -> ()
  )

let print_top pile =
  let opt = get_top pile in
  (match opt with
   |Some(Lettre(x)) -> x
   |None -> "Empty")

let rec check_lists l ll =
  match ll with
  | [] -> false
  | Lettre(h)::t -> if h = l then true else check_lists l t
                  
let check_init_state s states = 
    match states with
    |States(l) -> check_lists s l
  

let check_init_stack top stack = 
    match stack with
    | Stacksymbols(l) -> check_lists top l
    

let get_first_letter mot =
  if (String.length mot) > 0 then String.sub (mot) 0 (1)
  else
    ""
;;

let update_mot () = 
  if (String.length !curr_mot) > 1 then (update_value curr_mot (String.sub (!curr_mot) 1 ((String.length !curr_mot)-1))) 
  else 
    if (String.length !curr_mot) <= 1 then (update_value curr_mot "") else () 
;;

let all_verification () = 
  if (check_init_state !curr_state !all_states ) = false then failwith "état initial erroné"
  else
    let tmp = get_top !curr_pile in
    match tmp with
      | None -> ()
      | Some(Lettre(x)) -> if (check_init_stack x !all_stacksymbols) = false then failwith "symbole de pile initial erroné" else ()
;;

let manage_err () =
  if Stack.is_empty !curr_pile && (String.length !curr_mot) > 0 then
    failwith "pile vide mais mot non entierement consomme"
  else
    if (Stack.is_empty !curr_pile)=false && (String.length !curr_mot) = 0 then
      failwith "mot entierement consomme mais pile vide"
    else
      if (Stack.is_empty !curr_pile)=false && (String.length !curr_mot) > 0 then
        failwith "aucune transition applicable"
      else
        ()
;;

(* action selon operation *)
let rec evalInstr instr =
  match instr with
  | [] -> ()
  | Push(x)::t -> empile x !curr_pile ; evalInstr t
  | Pop::t -> depile_unit !curr_pile ; evalInstr t
  | Reject::t -> failwith "reject" 
  | Change(Lettre(x))::t -> update_value curr_state x ; evalInstr t
;;

let condition_met_next val1 =
  match val1 with
  | "_" -> true
  | _ -> 
    if (get_first_letter !curr_mot) = val1 then
      begin
        update_mot() ; true
      end
    else false

;;

let condition_met_state val1 =
  if !curr_state = val1 then true
  else false
;;

let condition_met_top val1 =
  let top = get_top !curr_pile in
  (match top with
   |Some(x) -> if x = val1 then true
               else false
   |None -> false
  )
;;

(* teste si la condition est bien respectee selon l'operation demandee *)
let condition_met casename val1 =
  match casename,val1 with
  | Next,Lettre(x) -> condition_met_next x
  | Top,val1 -> condition_met_top val1
  | State,Lettre(x) -> condition_met_state x
;;

   
(*condition*)
let evalCond casename cond =
  match cond with
  |(x, instr) -> let b = condition_met casename x in if b then 
    begin 
      evalInstr instr;
      true
    end
    else false
;;

(* boolean pour savoir s'il y a eu un changement *)
let rec evalCondList casename condlist case =
  match condlist with
  | [] -> manage_err() ; print_string("FIN\n"); false;
  |Condition(h1,h2)::t ->
    
    print_string("Mot "^(!curr_mot)^"\t");
    print_string("Etat "^(!curr_state)^"\t");
    print_string("Stack ") ; print_stack print_lettre !curr_pile ; print_string("\n***\n");

    let b = evalCond casename (h1,h2) in if b = false then evalCondList casename t case else true

  |CondCases(h1,h2)::t ->
    let b = condition_met casename h1 in
    if b then
      begin
        evalCases h2 ;
      end
    
    else evalCondList casename t case

and evalCases case =
  match case with
  |Cases(casename, l) ->
    let b = evalCondList casename l case in
    if b then evalCases !cases
    else false 
;;

let execute ast mot =
  update_value curr_mot mot;
  get_automate ast ;
  all_verification ();
  evalCases !cases
;;

