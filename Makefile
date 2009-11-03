SOURCES=util.mli util.ml player.ml board.mli board.ml game.mli game.ml
OBJECTS=util.cmo player.cmo board.cmo game.cmo

all: run_tests tbui

run_tests: tests
	./run_tests

run_tbui: tbui
	./tbui

tbui: $(OBJECTS) tbui.cmo
	ocamlfind ocamlc -o tbui -g -linkpkg $(OBJECTS) tbui.cmo

tests: $(OBJECTS) test.cmo
	ocamlfind ocamlc -o run_tests -g -linkpkg -package oUnit $(OBJECTS) test.cmo

$(OBJECTS) : $(SOURCES)
	ocamlc -c -g $(SOURCES)

test.cmo: $(OBJECTS) test.ml
	ocamlfind ocamlc -c -g -package oUnit test.ml

tbui.cmo: tbui.ml
	ocamlc -c -g tbui.ml

clean:
	rm *.cmi *.cmo run_tests tbui
