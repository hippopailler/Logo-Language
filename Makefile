all: logo

logo: ast.ml parser.mli parser.ml lexer.ml exec.ml main.ml
	ocamlfind ocamlc -package graphics -linkpkg ast.ml parser.mli parser.ml lexer.ml exec.ml main.ml -o logo

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean:
	rm -f *.cmi *.cmo *.mli parser.ml lexer.ml logo *~

.PHONY: all clean