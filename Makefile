all: spec

clean:
	rm *.cm* *.byte *.a *.o *.exe

game.cma: piece.ml column.mli column.ml board.mli board.ml player.ml ai.ml game.ml
	$(OCAMLC) -a -package batteries piece.ml column.mli column.ml board.mli board.ml player.ml ai.ml game.ml -o game.cma

game.cmxa: piece.ml column.mli column.ml board.mli board.ml player.ml ai.ml game.ml
	$(OCAMLOPT) -a -package batteries piece.ml column.mli column.ml board.mli board.ml player.ml ai.ml game.ml -o game.cmxa

spec.byte: game.cma spec.ml
	$(OCAMLC) -thread -package batteries,ospecl -linkpkg game.cma spec.ml -o spec.byte

spec: spec.byte
	ocamlrun -b ./spec.byte

tui.byte: game.cma tui.ml
	$(OCAMLC) -thread -package batteries -linkpkg game.cma tui.ml -o tui.byte

tui.exe: game.cmxa tui.ml
	$(OCAMLOPT) -thread -package batteries -linkpkg game.cmxa tui.ml -o tui.exe

gui.byte: game.cma gui.ml
	$(OCAMLC) -thread -package batteries,labltk -linkpkg game.cma gui.ml -o gui.byte

.PHONY: all clean spec

OCAMLC = ocamlfind ocamlc -g
OCAMLOPT = ocamlfind ocamlopt -g
