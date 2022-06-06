{
open Parser
}

let layout = [ ' ' '\t' '\n' ]
let lettre = ['0'-'9''a'-'z''A'-'Z''_']
let case_next = "case next of"
let case_top = "case top of"
let case_state = "case state of"

let epsilon = ""

rule main = parse
  | layout		{ main lexbuf }
  | ":" {COLON}
  | "," {COMMA}
  | ";" {SEMICOLON}
  | ""      {EPSILON (Lexing.lexeme lexbuf)}
  | "program" {PROGRAM}
  | "begin" {BEGIN}
  | "end" {END}
  | "pop" {POP}
  | "push" {PUSH}
  | "reject" {REJECT}
  | "change" {CHANGE}
  | "initial state" {INIT_STATE}
  | "initial stack symbol" {INIT_STACK}
  | "input symbols" {INPUT_SYMB}
  | "stack symbols" {STACK_SYMB}
  | "states" {STATES}
  | case_next {CASE_NEXT (Lexing.lexeme lexbuf)}
  | case_top {CASE_TOP (Lexing.lexeme lexbuf)}
  | case_state {CASE_STATE (Lexing.lexeme lexbuf)}
  | lettre		{ LETTRE (Lexing.lexeme lexbuf) }
  | eof			{ EOF }
  | _			{ failwith "unexpected character" }

