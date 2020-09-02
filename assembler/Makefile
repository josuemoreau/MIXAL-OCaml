EXEC := assembler

OBJ := op.cmx ast.cmx parser.cmx lexer.cmx word.cmx memory.cmx \
			machine.cmx instr.cmx main.cmx

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
	rm -f *.cmx
	rm -f *.o
	rm -f *.cmi
	rm -f lexer.ml parser.mli parser.ml
	rm -f $(EXEC)

.depend: parser.mli parser.ml lexer.ml
	ocamldep *.mli *.ml > .depend

include .depend
