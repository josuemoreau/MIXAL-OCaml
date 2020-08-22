EXEC := assembler

MLL := assembler.mll
ML  := assembler.ml
OBJ := Word.cmx assembler.cmx

.PRECIOUS: %.cmi
.DEFAULT_GOAL := $(EXEC)

assembler.cmx: $(MLL)
	ocamllex $^
	ocamlopt $(ML) -c

%.cmi: %.mli
	ocamlopt $^ -c

%.cmx: %.ml %.cmi
	ocamlopt $< -c

$(EXEC): $(OBJ)
	ocamlopt $(OBJ) -o $(EXEC)

all: $(EXEC)

clean:
	rm *.cmx
	rm *.o
	rm *.cmi
	rm $(EXEC)
