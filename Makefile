all: run_tests

run_tests: build_tests
	./run_tests

build_tests: test.cmo
	ocamlfind ocamlc -o run_tests -g -linkpkg -package oUnit util.cmo board.cmo test.cmo

util.cmo:
	ocamlc -c -g util.mli
	ocamlc -c -g util.ml

board.cmo: util.cmo
	ocamlc -c -g board.mli
	ocamlc -c -g board.ml

test.cmo: util.cmo board.cmo
	ocamlfind ocamlc -c -g -package oUnit test.ml

clean:
	rm *.cmi *.cmo run_tests
