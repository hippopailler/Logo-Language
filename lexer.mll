{
open Parser        
open Lexing

exception Error of string
}

rule token = parse
  | [' ' '\t' '\n']     { token lexbuf }    (* Ignore les espaces et retours à la ligne *)
  | "//" [^ '\n']* '\n' { token lexbuf }    (* Ignore les commentaires *)
  | ['0'-'9']+ as i     { NUMBER(int_of_string i) }
  | "forward"           { FORWARD }
  | "right"            { RIGHT }
  | "left"             { LEFT }
  | "penup"            { PENUP }
  | "pendown"          { PENDOWN }
  | "clear"            { CLEAR }
  | "repeat"           { REPEAT }
  | "setcolor"         { SETCOLOR }
  | "setpensize"      { SETPENSIZE }
  | "square"           { SQUARE }
  | "circle"           { CIRCLE }
  | "set"              { SET }
  | "mod"              { MOD }
  | "if"               { IF }
  | '['                { LBRACKET }
  | ']'                { RBRACKET }
  | '+'                { PLUS }
  | '-'                { MINUS }
  | '*'                { TIMES }
  | '/'                { DIV }
  | '='                { EQUAL }
  | '<'                { LT }
  | '>'                { GT }
  | "<>"               { NEQ }
  | "<="               { LE }
  | ">="               { GE }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id { IDENT(id) }
  | eof                { EOF }
  | _                  { raise (Error ("Caractère non reconnu: " ^ Lexing.lexeme lexbuf)) }