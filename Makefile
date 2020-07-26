EXEC := mixalint

MLI := Word.mli Char.mli Op.mli
ML  := Word.ml Char.ml Op.ml Instruction.ml

SIG := Word.cmi Char.cmi Op.cmi Instruction.cmi
OBJ := Word.cmx Char.cmx Op.cmx Instruction.cmx

%.cmx: %.mli %.ml
	ocamlopt $^ -c

$(EXEC): $(OBJ)
	ocamlopt $(OBJ) -o $(EXEC)

all: $(EXEC)
