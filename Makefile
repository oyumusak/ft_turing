TARGET=_build/default/main.exe

all:
	ocamlfind ocamlc -package yojson -linkpkg -o ft_turing main.ml

new:
	dune build main.exe

run: $(TARGET)
	dune exec $(TARGET) $(FILE) $(INPUT)

clean:
	dune clean