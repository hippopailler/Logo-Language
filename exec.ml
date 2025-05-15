open Graphics
open Ast

type state = {
  mutable x : float;
  mutable y : float;
  mutable angle : float;
  mutable pen : bool;
  mutable variables : (string * int) list;  (* Nouvelle liste pour stocker les variables *)
}

let deg_to_rad d = d *. Float.pi /. 180.

let rec exec_command state cmd =
  match cmd with
  | Forward n ->
      let dx = float_of_int n *. cos (deg_to_rad state.angle) in
      let dy = float_of_int n *. sin (deg_to_rad state.angle) in
      let x' = state.x +. dx in
      let y' = state.y +. dy in
      if state.pen then lineto (int_of_float x') (int_of_float y')
      else moveto (int_of_float x') (int_of_float y');
      state.x <- x'; state.y <- y'
  | Right a -> state.angle <- state.angle -. float_of_int a
  | Left a -> state.angle <- state.angle +. float_of_int a
  | PenUp -> state.pen <- false
  | PenDown -> state.pen <- true
  | SetColor c -> 
      let color = match c with
        | 0 -> black
        | 1 -> white
        | 2 -> red
        | 3 -> green
        | 4 -> blue
        | 5 -> yellow
        | 6 -> cyan
        | 7 -> magenta
        | _ -> black
      in
      set_color color
  | Clear -> clear_graph ()
  | Repeat (n, cmds) ->
      for _ = 1 to n do
        List.iter (exec_command state) cmds
      done
  | Square n ->
      for _ = 1 to 4 do
        exec_command state (Forward n);
        exec_command state (Right 90)
      done
  | SetVar (name, value) ->
      state.variables <- (name, value) :: 
        (List.remove_assoc name state.variables)
  | GetVar name ->
      let value = 
        try List.assoc name state.variables
        with Not_found -> failwith ("Variable non définie: " ^ name)
      in
      exec_command state (Square value)  (* Utilise la valeur pour exécuter la commande square *)
  | ModVar (name, op, value) ->
      let current_value = 
        try List.assoc name state.variables
        with Not_found -> failwith ("Variable non définie: " ^ name)
      in
      let new_value = match op with
        | '+' -> current_value + value
        | '-' -> current_value - value
        | _ -> failwith ("Opérateur non supporté: " ^ String.make 1 op)
      in
      state.variables <- (name, new_value) :: 
        (List.remove_assoc name state.variables)
  | _ -> failwith "Unknown command"

let exec_commands cmds =
  open_graph " 1000x1000";
  let state = { 
    x = 500.0; 
    y = 500.0; 
    angle = 0.0; 
    pen = true;
    variables = []  (* Initialisation de la liste des variables *)
  } in
  moveto (int_of_float state.x) (int_of_float state.y);
  List.iter (exec_command state) cmds;
  ignore (read_line ()) (* pause *)
