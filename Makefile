all: spec

clean:
	rm *.cm* *.byte *.a *.o *.exe

game.cma: piece.ml row.ml col.ml columns.mli columns.ml line.mli line.ml lines.mli lines.ml board.mli board.ml player.ml ai.ml game.ml
	$(OCAMLC) -a -package batteries piece.ml row.ml col.ml columns.mli columns.ml line.mli line.ml lines.mli lines.ml board.mli board.ml player.ml ai.ml game.ml -o game.cma

game.cmxa: piece.ml row.ml col.ml columns.mli columns.ml line.mli line.ml lines.mli lines.ml board.mli board.ml player.ml ai.ml game.ml
	$(OCAMLOPT) -a -package batteries piece.ml row.ml col.ml columns.mli columns.ml line.mli line.ml lines.mli lines.ml board.mli board.ml player.ml ai.ml game.ml -o game.cmxa

spec: game.cma spec.ml
	ospecl spec.ml

tui.byte: game.cma tui.ml
	$(OCAMLC) -thread -package batteries -linkpkg game.cma tui.ml -o tui.byte

tui.exe: game.cmxa tui.ml
	$(OCAMLOPT) -thread -package batteries -linkpkg game.cmxa tui.ml -o tui.exe

gui.byte: game.cma gui.ml
	$(OCAMLC) -thread -package batteries,labltk -linkpkg game.cma gui.ml -o gui.byte

.PHONY: all clean spec

OCAMLC = ocamlfind ocamlc -g
OCAMLOPT = ocamlfind ocamlopt -g
