all:
	ocamlfind ocamlopt -o board -linkpkg -package oUnit board.ml
clean:
	rm board.cmx board.cmi board.o board
