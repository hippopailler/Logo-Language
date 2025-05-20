{
open Parser        
open Lexing

exception Error of string
}

rule token = parse
  | [' ' '\t' '\n']     { token lexbuf }
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
  | "set"              { SET }
  | "mod"              { MOD }
  | "if"               { IF }
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
  | eof                { EOF }
  | _                  { raise (Error ("Caractère non reconnu: " ^ Lexing.lexeme lexbuf)) }