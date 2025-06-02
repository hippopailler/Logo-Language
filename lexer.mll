{
open Parser
open Lexing

exception LexError of (Lexing.position * Lexing.position)
let line_number = ref 0

let incr_line_number lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum }
}

let blank = [' ' '\t' '\r']
let newline = '\n' | "\r\n"
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | digit | '_')*

rule token = parse
  | newline               { incr_line_number lexbuf; token lexbuf }
  | blank+               { token lexbuf }
  | "//" [^ '\n']* '\n' { token lexbuf }
  | ['0'-'9']+ as i     { NUMBER(int_of_string i) }
  | "def"               { DEF }
  | "forward"           { FORWARD }
  | "right"            { RIGHT }
  | "left"             { LEFT }
  | "penup"            { PENUP }
  | "pendown"          { PENDOWN }
  | "clear"            { CLEAR }
  | "repeat"           { REPEAT }
  | "setcolor"         { SETCOLOR }
  | "square"           { SQUARE }
  | "circle"           { CIRCLE }
  | "triangle"         { TRIANGLE }
  | "set"              { SET }
  | "mod"              { MOD }
  | "if"               { IF }
  | "save"              { SAVE }
  | "restore"           { RESTORE }
  | "random"            { RANDOM }
  | '{'                { LBRACE }
  | '}'                { RBRACE }
  | '['                { LBRACKET }
  | ']'                { RBRACKET }
  | '('                { LPAREN }
  | ')'                { RPAREN }
  | ','                { COMMA }
  | '+'                { PLUS }
  | '-'                { MINUS }
  | '*'                { TIMES }
  | '/'                { DIV }
  | '='                { EQUAL }
  | "<>"               { NEQ }
  | '<'                { LT }
  | '>'                { GT }
  | "<="               { LE }
  | ">="               { GE }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id { IDENT(id) }
  | eof                  { EOF }
  | _                    { raise (LexError (lexbuf.lex_start_p, lexbuf.lex_curr_p)) }