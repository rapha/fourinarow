all: run_tests

run_tests: build_tests
	./run_tests

build_tests: test_board.cmo
	ocamlfind ocamlc -o run_tests -linkpkg -package oUnit util.cmo board.cmo test_board.cmo

util.cmo:
	ocamlc -c util.mli
	ocamlc -c util.ml

board.cmo: util.cmo
	ocamlc -c board.mli
	ocamlc -c board.ml 

test_board.cmo: util.cmo board.cmo
	ocamlfind ocamlc -c -package oUnit test_board.ml 

clean:
	rm *.cmi *.cmo run_tests
