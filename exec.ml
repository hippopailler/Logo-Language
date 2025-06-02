open Graphics
open Ast

type saved_state = {
    x : float;
    y : float;
    angle : float;
    pen : bool;
}

type state = {
  mutable x : float;
  mutable y : float;
  mutable angle : float;
  mutable pen : bool;
  mutable variables : (string * int) list;
  mutable saved_states : saved_state list;  (* Nouvelle pile d'états *)
}

let functions = Hashtbl.create 17

let deg_to_rad d = d *. Float.pi /. 180.

let rec eval_expr state = function
  | Econst n -> n
  | Evar v -> 
      (try List.assoc v state.variables 
       with Not_found -> failwith ("Variable non définie: " ^ v))
  | Erandom e ->      (* Déplacer cette clause ici, au niveau des expressions *)
      let n = eval_expr state e in
      if n <= 0 then 0 else Random.int n
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
  | SetPenSize e ->
      let size = eval_expr state e in
      set_line_width size
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
  | Circle e ->
      let n = eval_expr state e in
      let radius = float_of_int n in
      let steps = 360 in
      for i = 0 to steps - 1 do
        let angle = float_of_int i *. (2.0 *. Float.pi /. float_of_int steps) in
        let x' = state.x +. radius *. cos angle in
        let y' = state.y +. radius *. sin angle in
        if state.pen then lineto (int_of_float x') (int_of_float y')
        else moveto (int_of_float x') (int_of_float y');
      done;
  | Triangle e ->
      let n = eval_expr state e in
      for _ = 1 to 3 do
        exec_command state (Forward (Econst n));
        exec_command state (Right (Econst 120))
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
  | Procedure (name, params, body) ->
      Hashtbl.add functions name (params, body)
  | Call (name, args) ->
      let params, body =
        try Hashtbl.find functions name
        with Not_found -> failwith ("Fonction non définie : " ^ name)
      in
      if List.length params <> List.length args then
        failwith ("Nombre d'arguments incorrect pour la fonction : " ^ name);
      let local_vars =
        List.combine params (List.map (eval_expr state) args)
      in
      let old_vars = state.variables in
      state.variables <- local_vars @ state.variables;
      List.iter (exec_command state) body;
      state.variables <- old_vars
  | Save ->
        let current = {
            x = state.x;
            y = state.y;
            angle = state.angle;
            pen = state.pen;
        } in
        state.saved_states <- current :: state.saved_states

    | Restore ->
        match state.saved_states with
        | [] -> failwith "Erreur : aucun état sauvegardé"
        | s :: rest ->
            state.x <- s.x;
            state.y <- s.y;
            state.angle <- s.angle;
            state.pen <- s.pen;
            state.saved_states <- rest;
            moveto (int_of_float state.x) (int_of_float state.y)

let exec_commands cmds =
    Random.self_init (); 
    open_graph " 1000x1000";
    let state = { 
        x = 500.0; 
        y = 500.0; 
        angle = 0.0; 
        pen = true;
        variables = [];
        saved_states = [] 
    } in
    moveto (int_of_float state.x) (int_of_float state.y);
    List.iter (exec_command state) cmds;
    ignore (read_line ())
