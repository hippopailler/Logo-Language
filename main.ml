let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s <file.logo>\n" Sys.argv.(0);
    exit 1
  );
  let cmds = Parser.parse_file Sys.argv.(1) in
  Exec.exec_commands cmds
