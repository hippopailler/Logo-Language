%{
open Ast
%}

%token <int> NUMBER
%token <string> IDENT
%token FORWARD RIGHT LEFT PENUP PENDOWN CLEAR
%token REPEAT SETCOLOR SQUARE CIRCLE SET MOD IF
%token LBRACKET RBRACKET
%token PLUS MINUS TIMES DIV
%token EQUAL NEQ LT GT LE GE
%token EOF
%token SETPENSIZE
%token FILL

%left PLUS MINUS
%left TIMES DIV

%start program
%type <Ast.command list> program

%%

program:
    list EOF                { $1 }
;

list:
  | /* empty */            { [] }
  | command list          { $1 :: $2 }
;

command:
  | FORWARD expr          { Forward($2) }
  | RIGHT expr           { Right($2) }
  | LEFT expr            { Left($2) }
  | PENUP               { PenUp }
  | PENDOWN             { PenDown }
  | CLEAR               { Clear }
  | SETCOLOR expr       { SetColor($2) }
  | SETPENSIZE expr     { SetPenSize($2) }
  | SQUARE expr         { Square($2) }
  | CIRCLE expr         { Circle($2) }
  | SET IDENT expr      { SetVar($2, $3) }
  | MOD IDENT op NUMBER { ModVar($2, $3, Econst($4)) }
  | REPEAT expr LBRACKET list RBRACKET 
                        { Repeat($2, $4) }
  | IF expr LBRACKET list RBRACKET 
                        { If($2, $4) }
;

expr:
  | NUMBER              { Econst($1) }
  | IDENT              { Evar($1) }
  | expr PLUS expr     { Ebinop(Add, $1, $3) }
  | expr MINUS expr    { Ebinop(Sub, $1, $3) }
  | expr TIMES expr    { Ebinop(Mul, $1, $3) }
  | expr DIV expr      { Ebinop(Div, $1, $3) }
  | expr EQUAL expr    { Ebinop(Eq, $1, $3) }
  | expr NEQ expr      { Ebinop(Neq, $1, $3) }
  | expr LT expr       { Ebinop(Lt, $1, $3) }
  | expr GT expr       { Ebinop(Gt, $1, $3) }
  | expr LE expr       { Ebinop(Le, $1, $3) }
  | expr GE expr       { Ebinop(Ge, $1, $3) }
;

op:
  | PLUS               { '+' }
  | MINUS              { '-' }
;