type command =
  | Forward of int
  | Right of int
  | Left of int
  | PenUp
  | PenDown
  | Clear
  | Repeat of int * command list
  | SetColor of int
  | Square of int
  | SetVar of string * int  (* Nouvelle commande pour définir une variable *)
  | GetVar of string        (* Nouvelle commande pour utiliser une variable *)
  | ModVar of string * char * int  (* Nouvelle commande pour modifier une variable: nom, opérateur (+/-), valeur *)