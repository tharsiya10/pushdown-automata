open Ast
open String
open Stack
open Array

(*Variables globales:
curr_state / curr_pile / curr_mot*)

let curr_mot = ref("") ;;
let curr_state = ref("") ;;
let curr_pile = ref(Stack.create());;
let transitions = ref(Transitions(Translist([])));;
let change = ref(false);;
let all_stacksymbols = ref(Stacksymbols([]));;
let all_states = ref(States([]));;

(*print stack from top to bottom*)
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
  
(*recuperer donnees importantes*)
let get_automate a = 
  match a with
  | Automate (decl,trans) -> 
     (match decl,trans with
      | Declaration(_,stacksymb,states,state,stack),trans -> 
        all_stacksymbols := stacksymb;
        all_states := states;
        curr_state := get_init_state state ; 
        (push (get_init_stack stack) !curr_pile ) ;
        transitions:=trans ;
     ) 

let update_value var value = 
  var := value

let empile a pile = 
  Stack.push a pile

let depile pile = 
  if(Stack.is_empty pile) then None else Some (Stack.pop pile)
  
let get_top pile = 
  if(Stack.is_empty pile) then None else Some(Stack.top pile)
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
    
let rec check_transitionlist transitionlist x1' x2' x3' = 
  match transitionlist with
  | [] -> true
  | h::t -> 
    (match h with
      |Transition(Lettre(x1), Lettreouvide(x2), Lettre(x3), _ , _ )-> 
        if x1 = x1' && x2=x2' && x3=x3' then failwith "Automate non déterministe" else check_transitionlist t x1' x2' x3'

      |Transition(Lettre(x1), Epsilon, Lettre(x3), _ , _ )-> 
        if x1 = x1' && ""=x2' && x3=x3' then  failwith "Automate non déterministe" else check_transitionlist t x1' x2' x3'
      )

(*vérifier si une transition est unique*)      
let rec check_determinisme transitionlist = 
  match transitionlist with
    | [] -> true
    | h::t -> 
      (match h with
        | Transition(Lettre(x1), Lettreouvide(x2), Lettre(x3), _, _) -> 
          (check_transitionlist t x1 x2 x3) && (check_determinisme t)
        | Transition(Lettre(x1), Epsilon, Lettre(x3), _, _) -> 
          (check_transitionlist t x1 "" x3) && (check_determinisme t)
      )

let check_det t = 
  match t with
    | Transitions(Translist(tl)) -> check_determinisme tl

let rec get_length l =
  match l with
  |[] -> 0
  |h::t -> 1 + (get_length t)
;;
 
let rec get_last tab = 
  match tab with
  |[] -> failwith "liste vide"
  |[x] -> x
  |h::t -> get_last t

  (*retourne true si la condition pour effectuer la transition est respectee*)
let condition_met state toconsume t  =
  if !curr_state <> state then false
  else
    if toconsume <> "" && !curr_mot = "" then false
    else
      if toconsume <> "" && !curr_mot <> "" then
        if (String.get !curr_mot 0) = (String.get toconsume 0) then  
          let aux = (get_top !curr_pile) in 
          (match aux with
           |None -> if t = "" then true else false
           |Some(Lettre y) -> if y = t then true else false
          )
        else
          false
      else
        false              
;;

let rec empile_all e pile =
  match e with
  | [] -> ()
  | h::t -> empile h pile ; empile_all t pile
       

let get_stack s pile = 
  match s with
  | Stack(h) -> empile_all h pile

let update_mot () = 
  if (String.length !curr_mot) > 1 then (update_value curr_mot (String.sub (!curr_mot) 1 ((String.length !curr_mot)-1))) 
  else 
    if (String.length !curr_mot) <= 1 then (update_value curr_mot "") else () 
;;

(*executer une transition si condition respectee*)
let get_transition t =
  match t with
  | Transition(Lettre x1, Lettreouvide((x2)), Lettre(x3), Lettre x4,  s) -> let b = condition_met x1 x2 x3 in 
        if b then 
          begin 
            (update_value curr_state x4) ; 
            (update_mot() );
            let aux = depile !curr_pile in
            (match aux with
             |_ -> ());
            get_stack s !curr_pile;
            (update_value change true); 
          end 
  | Transition(Lettre x1, Epsilon, Lettre(x3), Lettre x4,  s) -> let b = condition_met x1 "" x3 in 
        if b then 
          (update_value curr_state x4);  
          let tmp = depile !curr_pile in
          (match tmp with
            | _ -> ());
          get_stack s !curr_pile;
          (update_value change true) 
;;

(*parcourir liste des transitions*)
let rec browse tt tl = 
  match tl with
  | [] -> failwith "aucune transition applicable"
  | [h] -> get_transition h;
           if(!change) = false then failwith "aucune transition applicable"
           else
             if(is_empty !curr_pile) && (!curr_mot = "") then 
               begin
                 print_string("mot: "^(!curr_mot)^"\tetat: "^(!curr_state)^"\tpile(top to bottom): ");
                 print_stack print_lettre !curr_pile ; print_string("\n");                 
               end
             else
               if(is_empty !curr_pile) then failwith "pile vide mais mot non consommé"
               else
                 failwith "mot consommé intégralement mais pile non vide"
        
  | h::t -> get_transition h; 
            if(is_empty !curr_pile) && (!curr_mot = "") then print_string("FINI")
            else 
              if(!change) = false then 
                begin
                  browse tt t;
                end
              else
                begin
                  (update_value change false) ;
                  print_string("mot: "^(!curr_mot)^"\tetat: "^(!curr_state)^"\tpile(top to bottom): ");
                  print_stack print_lettre !curr_pile ; print_string("\n"); 
                 
                  browse tt tt;
                end    

(*verification partie declarative*)
let all_verification () = 
  if (check_init_state !curr_state !all_states ) = false then failwith "état initial erroné"
  else
    let tmp = get_top !curr_pile in
    match tmp with
      | None -> true
      | Some(Lettre(x)) -> if (check_init_stack x !all_stacksymbols) = false then failwith "symbole de pile initial erroné" else true
 
let execute a mot =
  update_value curr_mot mot;
  get_automate a ; 
  if all_verification() && check_det !transitions then
    print_string("mot: "^(!curr_mot)^"\tetat: "^(!curr_state)^" ");
    print_stack print_lettre !curr_pile ; print_string("\n"); 
    (match !transitions with
      | Transitions(Translist(tl)) -> browse tl tl);
