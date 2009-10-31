all: run_tests

run_tests: build_tests
	./run_tests

build_tests: test_board.cmo
	ocamlfind ocamlc -o run_tests -g -linkpkg -package oUnit util.cmo board.cmo test_board.cmo

util.cmo:
	ocamlc -c -g util.mli
	ocamlc -c -g util.ml

board.cmo: util.cmo
	ocamlc -c -g board.mli
	ocamlc -c -g board.ml

test_board.cmo: util.cmo board.cmo
	ocamlfind ocamlc -c -g -package oUnit test_board.ml

clean:
	rm *.cmi *.cmo run_tests
