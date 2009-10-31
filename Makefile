all: run_tests

run_tests: build_tests
	./run_tests

build_tests: board.cmo test_board.cmo
	ocamlfind ocamlc -o run_tests -linkpkg -package oUnit board.cmo test_board.cmo

board.cmo:
	ocamlc -c board.ml 

test_board.cmo:
	ocamlfind ocamlc -c -package oUnit test_board.ml 

clean:
	rm *.cmi *.cmo run_tests
