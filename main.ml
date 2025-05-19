let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try
    let result = Parser.program Lexer.token lexbuf in
    close_in chan;
    result
  with e ->
    close_in chan;
    raise e

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s <file.logo>\n" Sys.argv.(0);
    exit 1
  );
  let cmds = parse_file Sys.argv.(1) in
  Exec.exec_commands cmds
