all: spec

clean:
	rm *.cm* *.byte

game.cma: piece.ml column.mli column.ml board.mli board.ml player.ml ai.ml game.ml
	$(OCAMLC) -a -package batteries piece.ml column.mli column.ml board.mli board.ml player.ml ai.ml game.ml -o game.cma

spec.byte: game.cma spec.ml
	$(OCAMLC) -thread -package batteries,ospecl -linkpkg game.cma spec.ml -o spec.byte

spec: spec.byte
	ocamlrun -b ./spec.byte

tui.byte: game.cma tui.ml
	$(OCAMLC) -thread -package batteries -linkpkg game.cma tui.ml -o tui.byte

gui.byte: game.cma gui.ml
	$(OCAMLC) -thread -package batteries,labltk -linkpkg game.cma gui.ml -o gui.byte

.PHONY: all clean spec

OCAMLC = ocamlfind ocamlc -g
