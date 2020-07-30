EXEC := mixalint

MLI := Word.mli Char.mli Op.mli Instruction.mli Parser.mli
ML  := Word.ml Char.ml Op.ml Instruction.ml Parser.ml

SIG := Word.cmi Char.cmi Op.cmi Instruction.cmi Parser.cmi
OBJ := Word.cmx Char.cmx Op.cmx Instruction.cmx Parser.cmx test.cmx

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
