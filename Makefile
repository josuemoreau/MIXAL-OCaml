EXEC := mixalint

MLI := Word.mli Char.mli Op.mli Instruction.mli
ML  := Word.ml Char.ml Op.ml Instruction.ml

SIG := Word.cmi Char.cmi Op.cmi Instruction.cmi
OBJ := Word.cmx Char.cmx Op.cmx Instruction.cmx test.cmx

test.cmx: test.ml
	ocamlopt $^ -c

%.cmx: %.mli %.ml
	ocamlopt $^ -c

$(EXEC): $(ML) $(MLI) $(OBJ)
	ocamlopt $(OBJ) -o $(EXEC)

all: $(EXEC)

clean:
	rm *.cmx
	rm *.o
	rm *.cmi
	rm $(EXEC)
