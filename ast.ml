type binop = Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | Le | Ge

type expr =
  | Econst of int
  | Evar of string
  | Ebinop of binop * expr * expr

type command =
  | Forward of expr
  | Right of expr
  | Left of expr
  | PenUp
  | PenDown
  | Clear
  | Repeat of expr * command list
  | SetColor of expr
  | Square of expr
  | Circle of expr
  | SetVar of string * expr
  | ModVar of string * char * expr
  | If of expr * command list
  | SetPenSize of expr

