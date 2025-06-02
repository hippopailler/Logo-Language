let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let result = Parser.program Lexer.token lexbuf in
    close_in chan;
    result
  with
  | Parsing.Parse_error ->
      let sp = Lexing.lexeme_start_p lexbuf in
      let ep = Lexing.lexeme_end_p lexbuf in
      Format.eprintf 
        "@[File %S, line %i, characters %i-%i:@ Syntax error.@]@\n"
        sp.pos_fname
        sp.pos_lnum
        (sp.pos_cnum - sp.pos_bol)
        (ep.pos_cnum - ep.pos_bol);
      exit 1
  | Lexer.LexError (sp, ep) ->
      Format.eprintf 
        "@[File %S, line %i, characters %i-%i:@ Lexical error.@]@\n"
        sp.pos_fname
        sp.pos_lnum
        (sp.pos_cnum - sp.pos_bol)
        (ep.pos_cnum - ep.pos_bol);
      exit 1

let () =
  if Array.length Sys.argv < 2 then (
    Format.eprintf "Usage: %s <filename>@." Sys.argv.(0);
    exit 1
  );
  let cmds = parse_file Sys.argv.(1) in
  Exec.exec_commands cmds
