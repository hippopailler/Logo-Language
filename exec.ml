open Graphics
open Ast

type state = {
  mutable x : float;
  mutable y : float;
  mutable angle : float;
  mutable pen : bool;
  mutable variables : (string * int) list;
}

let deg_to_rad d = d *. Float.pi /. 180.

let rec eval_expr state = function
  | Econst n -> n
  | Evar v -> 
      (try List.assoc v state.variables 
       with Not_found -> failwith ("Variable non définie: " ^ v))
  | Ebinop (op, e1, e2) ->
      let v1 = eval_expr state e1 in
      let v2 = eval_expr state e2 in
      match op with
      | Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2
      | Eq  -> if v1 = v2 then 1 else 0
      | Neq -> if v1 <> v2 then 1 else 0
      | Lt  -> if v1 < v2 then 1 else 0
      | Gt  -> if v1 > v2 then 1 else 0
      | Le  -> if v1 <= v2 then 1 else 0
      | Ge  -> if v1 >= v2 then 1 else 0

let rec exec_command state cmd =
  match cmd with
  | Forward e ->
      let n = eval_expr state e in
      let dx = float_of_int n *. cos (deg_to_rad state.angle) in
      let dy = float_of_int n *. sin (deg_to_rad state.angle) in
      let x' = state.x +. dx in
      let y' = state.y +. dy in
      if state.pen then lineto (int_of_float x') (int_of_float y')
      else moveto (int_of_float x') (int_of_float y');
      state.x <- x'; state.y <- y'
  | Right e ->
      let a = eval_expr state e in
      state.angle <- state.angle -. float_of_int a
  | Left e ->
      let a = eval_expr state e in
      state.angle <- state.angle +. float_of_int a
  | PenUp -> state.pen <- false
  | PenDown -> state.pen <- true
  | SetColor e ->
      let c = eval_expr state e in
      let color = match c with
        | 0 -> black
        | 1 -> magenta
        | 2 -> red
        | 3 -> green
        | 4 -> blue
        | 5 -> yellow
        | 6 -> cyan
        | 7 -> white
        | _ -> black
      in
      set_color color
  | Clear -> clear_graph ()
  | Repeat (e, cmds) ->
      let n = eval_expr state e in
      for _ = 1 to n do
        List.iter (exec_command state) cmds
      done
  | Square e ->
      let n = eval_expr state e in
      for _ = 1 to 4 do
        exec_command state (Forward (Econst n));
        exec_command state (Right (Econst 90))
      done
  | SetVar (name, e) ->
      let value = eval_expr state e in
      state.variables <- (name, value) :: (List.remove_assoc name state.variables)
  | ModVar (name, op, e) ->
      let value = eval_expr state e in
      let current_value =
        try List.assoc name state.variables
        with Not_found -> failwith ("Variable non définie: " ^ name)
      in
      let new_value = match op with
        | '+' -> current_value + value
        | '-' -> current_value - value
        | _ -> failwith ("Opérateur non supporté: " ^ String.make 1 op)
      in
      state.variables <- (name, new_value) :: (List.remove_assoc name state.variables)
  | If (cond, cmds) ->
      if eval_expr state cond <> 0 then
        List.iter (exec_command state) cmds

let exec_commands cmds =
  open_graph " 1000x1000";
  let state = { 
    x = 500.0; 
    y = 500.0; 
    angle = 0.0; 
    pen = true;
    variables = [] 
  } in
  moveto (int_of_float state.x) (int_of_float state.y);
  List.iter (exec_command state) cmds;
  ignore (read_line ())
