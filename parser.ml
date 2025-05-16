open Ast

let parse_binop = function
  | "+" -> Add | "-" -> Sub | "*" -> Mul | "/" -> Div
  | "=" -> Eq | "<>" -> Neq | "<" -> Lt | ">" -> Gt | "<=" -> Le | ">=" -> Ge
  | op -> failwith ("Unknown operator: " ^ op)

let rec parse_expr s =
  (* À améliorer pour gérer la priorité des opérateurs *)
  let parts = String.split_on_char ' ' (String.trim s) in
  match parts with
  | [a] when String.for_all (fun c -> '0' <= c && c <= '9') a -> Econst (int_of_string a)
  | [a] -> Evar a
  | [a; op; b] -> Ebinop (parse_binop op, parse_expr a, parse_expr b)
  | _ -> failwith ("Cannot parse expr: " ^ s)

let rec parse_lines lines =
  match lines with
  | [] -> []
  | line :: rest ->
    let line = String.trim line in
    if line = "" then parse_lines rest
    else if line = "penup" then PenUp :: parse_lines rest
    else if line = "pendown" then PenDown :: parse_lines rest
    else if line = "clear" then Clear :: parse_lines rest
    else if String.length line >= 4 && String.sub line 0 4 = "set " then
      let parts = List.filter ((<>) "") (String.split_on_char ' ' line) in
      match parts with
      | ["set"; var_name; value] -> 
          let expr = parse_expr value in
          SetVar (var_name, expr) :: parse_lines rest
      | _ -> failwith ("Invalid set syntax: " ^ line)
    else if String.length line >= 7 && String.sub line 0 7 = "forward" then
      let arg = String.trim (String.sub line 8 ((String.length line) - 8)) in
      Forward (parse_expr arg) :: parse_lines rest
    else if String.length line >= 5 && String.sub line 0 5 = "right" then
      let arg = String.trim (String.sub line 6 ((String.length line) - 6)) in
      Right (parse_expr arg) :: parse_lines rest
    else if String.length line >= 4 && String.sub line 0 4 = "left" then
      let arg = String.trim (String.sub line 5 ((String.length line) - 5)) in
      Left (parse_expr arg) :: parse_lines rest
    else if String.length line >= 8 && String.sub line 0 8 = "setcolor" then
      let arg = String.trim (String.sub line 9 ((String.length line) - 9)) in
      SetColor (parse_expr arg) :: parse_lines rest
    else if String.length line >= 6 && String.sub line 0 6 = "repeat" then
      let parts = String.split_on_char ' ' line in
      let count_expr = parse_expr (List.nth parts 1) in
      let body, rest' = parse_repeat_body rest in
      Repeat (count_expr, body) :: parse_lines rest'
    else if String.length line >= 6 && String.sub line 0 6 = "square" then
      let parts = String.split_on_char ' ' line in
      match parts with
      | ["square"; arg] -> Square (parse_expr arg) :: parse_lines rest
      | _ -> failwith ("Invalid square syntax: " ^ line)
    else if String.length line >= 3 && String.sub line 0 3 = "mod" then
      let parts = List.filter ((<>) "") (String.split_on_char ' ' line) in
      match parts with
      | ["mod"; var_name; op; value] -> 
          (try 
            let n = int_of_string (String.trim value) in
            let operator = if String.length op = 1 then op.[0] else failwith "Invalid operator" in
            if operator <> '+' && operator <> '-' then failwith "Invalid operator";
            ModVar (var_name, operator, Econst n) :: parse_lines rest
          with _ -> failwith ("Invalid mod syntax: " ^ line))
      | _ -> failwith ("Invalid mod syntax: " ^ line)
      else if String.length line >= 2 && String.sub line 0 2 = "if" then
        let line_without_if = String.sub line 3 ((String.length line) - 3) in
        let parts = String.split_on_char '[' line_without_if in
        match parts with
        | [cond_str; _] ->
            let condition = String.trim cond_str in
            Printf.printf "DEBUG: Parsing condition: '%s'\n" condition;
            let cond = parse_expr condition in
            (* on ajoute une ligne fictive contenant juste '[' pour bien démarrer le bloc *)
            let body, rest' = parse_repeat_body ("[" :: rest) in
            If (cond, body) :: parse_lines rest'
        | [cond_only] when String.ends_with ~suffix:"[" cond_only ->
            let condition = String.trim (String.sub cond_only 0 (String.length cond_only - 1)) in
            let cond = parse_expr condition in
            let body, rest' = parse_repeat_body rest in
            If (cond, body) :: parse_lines rest'
        | _ ->
            failwith "Invalid if syntax: missing or malformed condition"
    else failwith ("Syntax error: " ^ line)

and parse_repeat_body lines =
  let rec aux acc depth = function
    | [] -> failwith "Missing closing bracket"
    | line :: rest ->
        let line = String.trim line in
        Printf.printf "DEBUG: Processing line '%s' at depth %d\n" line depth;
        if line = "" then 
          aux acc depth rest
        else if line = "]" then begin
          Printf.printf "DEBUG: Found closing bracket at depth %d\n" depth;
          if depth = 1 then (List.rev acc, rest)
          else aux acc (depth - 1) rest
        end
        else if line = "[" then begin
          Printf.printf "DEBUG: Found opening bracket at depth %d\n" depth;
          aux acc (depth + 1) rest
        end
        else if String.ends_with ~suffix:"[" line then begin
          (* Si la ligne se termine par '[', on la traite sans le crochet *)
          let line_without_bracket = String.sub line 0 (String.length line - 1) in
          if line_without_bracket <> "" then begin
            let cmds = parse_lines [line_without_bracket] in
            aux (cmds @ acc) (depth + 1) rest
          end else
            aux acc (depth + 1) rest
        end
        else begin
          let cmds = parse_lines [line] in
          aux (cmds @ acc) depth rest
        end
  in 
  Printf.printf "DEBUG: Starting block with depth 1\n";
  aux [] 1 lines

let parse_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    []
  with End_of_file ->
    close_in chan;
    parse_lines (List.rev !lines)

