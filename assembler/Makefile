EXEC := assembler

MLL := assembler.mll
ML  := assembler.ml
OBJ := assembler.cmx

%.ml: %.mll
	ocamllex $^

%.cmx: %.ml
	ocamlopt $^ -c

$(EXEC): $(MLL) $(ML) $(OBJ)
	ocamlopt $(OBJ) -o $(EXEC)

all: $(EXEC)

clean:
	rm *.cmx
	rm *.o
	rm *.cmi
	rm $(EXEC)
