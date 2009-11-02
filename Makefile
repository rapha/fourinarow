all: run_tests

run_tests: build_tests
	./run_tests

build_tests: test.cmo
	ocamlfind ocamlc -o run_tests -g -linkpkg -package oUnit util.cmo player.cmo board.cmo game.cmo test.cmo

util.cmo:
	ocamlc -c -g util.mli
	ocamlc -c -g util.ml

player.cmo:
	ocamlc -c -g player.ml

board.cmo: player.cmo util.cmo
	ocamlc -c -g board.mli
	ocamlc -c -g board.ml

game.cmo: player.cmo board.cmo
	ocamlc -c -g game.mli
	ocamlc -c -g game.ml

test.cmo: util.cmo board.cmo game.cmo
	ocamlfind ocamlc -c -g -package oUnit test.ml

clean:
	rm *.cmi *.cmo run_tests
