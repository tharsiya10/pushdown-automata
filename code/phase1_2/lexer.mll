{
open Parser
}

let layout = [ ' ' '\t' '\n' ]
let lettre = ['0'-'9''a'-'z''A'-'Z']
let epsilon = ""

rule main = parse
  | layout		{ main lexbuf }
  | ":" {COLON}
  | "," {COMMA}
  | ";" {SEMICOLON}
  | ")"			{ RPAREN }
  | '('			{ LPAREN }
  | ""      {EPSILON (Lexing.lexeme lexbuf)}
  | "initial state" {INIT_STATE}
  | "initial stack symbol" {INIT_STACK}
  | "input symbols" {INPUT_SYMB}
  | "stack symbols" {STACK_SYMB}
  | "states" {STATES}
  | "transitions" {TRANSITIONS}
  | lettre		{ LETTRE (Lexing.lexeme lexbuf) }
  | eof			{ EOF }
  | _			{ failwith "unexpected character" }

