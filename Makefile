all: test

clean:
	rm *.cm* *.byte

game.cma: piece.ml board.mli board.ml player.ml ai.ml game.ml
	ocamlfind ocamlc -a -thread -package batteries -linkpkg piece.ml board.mli board.ml player.ml ai.ml game.ml -o game.cma

test: game.cma test.ml
	ocamlfind ocamlc -thread -package batteries,oUnit -linkpkg game.cma test.ml -o test.byte
	./test.byte

tui: test
	ocamlfind ocamlc -thread -package batteries -linkpkg game.cma tui.ml -o tui.byte

gui: test
	ocamlfind ocamlc -thread -package batteries,labltk -linkpkg game.cma gui.ml -o gui.byte

