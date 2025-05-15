open Ast

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
      let parts = String.split_on_char ' ' line in
      match parts with
      | ["set"; var_name; value] -> 
          (try 
            let n = int_of_string value in
            SetVar (var_name, n) :: parse_lines rest
          with _ -> failwith ("Invalid number in set command: " ^ value))
      | _ -> failwith ("Invalid set syntax: " ^ line)
    else if String.length line >= 7 && String.sub line 0 7 = "forward" then
      let n = int_of_string (String.sub line 8 ((String.length line) - 8)) in
      Forward n :: parse_lines rest
    else if String.length line >= 5 && String.sub line 0 5 = "right" then
      let n = int_of_string (String.sub line 6 ((String.length line) - 6)) in
      Right n :: parse_lines rest
    else if String.length line >= 4 && String.sub line 0 4 = "left" then
      let n = int_of_string (String.sub line 5 ((String.length line) - 5)) in
      Left n :: parse_lines rest
    else if String.length line >= 8 && String.sub line 0 8 = "setcolor" then
      let n = int_of_string (String.sub line 9 ((String.length line) - 9)) in
      SetColor n :: parse_lines rest
    else if String.length line >= 6 && String.sub line 0 6 = "repeat" then
      let parts = String.split_on_char ' ' line in
      let count = int_of_string (List.nth parts 1) in
      let body, rest' = parse_repeat_body rest in
      Repeat (count, body) :: parse_lines rest'
    else if String.length line >= 6 && String.sub line 0 6 = "square" then
      let parts = String.split_on_char ' ' line in
      match parts with
      | ["square"; n] -> 
          (try 
            let value = int_of_string n in
            Square value :: parse_lines rest
          with _ -> Square 0 :: GetVar n :: parse_lines rest)
      | _ -> failwith ("Invalid square syntax: " ^ line)
    else if String.length line >= 3 && String.sub line 0 3 = "mod" then
      let parts = String.split_on_char ' ' line in
      match parts with
      | ["mod"; var_name; op; value] -> 
          (try 
            let n = int_of_string value in
            let operator = if String.length op = 1 then op.[0] else failwith "Invalid operator" in
            if operator <> '+' && operator <> '-' then failwith "Invalid operator";
            ModVar (var_name, operator, n) :: parse_lines rest
          with _ -> failwith ("Invalid mod syntax: " ^ line))
      | _ -> failwith ("Invalid mod syntax: " ^ line)
    else failwith ("Syntax error: " ^ line)

and parse_repeat_body lines =
  let rec aux acc = function
    | [] -> failwith "Missing closing bracket"
    | line :: rest ->
      if String.trim line = "]" then (List.rev acc, rest)
      else
        let cmds = parse_lines [line] in
        aux (cmds @ acc) rest
  in aux [] lines

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
