EXEC := assembler

OBJ := op.cmx ast.cmx parser.cmx lexer.cmx word.cmx main.cmx

.PRECIOUS: %.cmi
.DEFAULT_GOAL := $(EXEC)

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.mli parser.ml: parser.mly
	menhir -v parser.mly

$(EXEC): $(OBJ)
	ocamlopt $(OBJ) -o $(EXEC)

.SUFFIXES: .ml .mli .cmi .cmx

.mli.cmi:
	ocamlopt -c $<

.ml.cmx:
	ocamlopt -c $<

clean:
	rm *.cmx
	rm *.o
	rm *.cmi
	rm $(EXEC)

.depend:
	ocamldep *.mli *.ml > .depend

include .depend
