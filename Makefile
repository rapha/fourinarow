SOURCES=util.mli util.ml player.ml board.mli board.ml game.mli game.ml
OBJECTS=util.cmo player.cmo board.cmo game.cmo

all: run_tests tbui gui

run_tests: tests
	./run_tests

gui: $(OBJECTS) gui.cmo
	ocamlfind batteries/ocamlc -o gui -g -linkpkg $(OBJECTS) -package labltk gui.cmo

tbui: $(OBJECTS) tbui.cmo
	ocamlfind batteries/ocamlc -o tbui -g -linkpkg $(OBJECTS) tbui.cmo

tests: $(OBJECTS) test.cmo
	ocamlfind batteries/ocamlc -o run_tests -g -linkpkg -package oUnit $(OBJECTS) test.cmo

$(OBJECTS) : $(SOURCES)
	ocamlfind batteries/ocamlc -c -g $(SOURCES)

test.cmo: $(OBJECTS) test.ml
	ocamlfind batteries/ocamlc -c -g -package oUnit test.ml

tbui.cmo: tbui.ml
	ocamlfind batteries/ocamlc -c -g tbui.ml

gui.cmo: gui.ml
	ocamlfind batteries/ocamlc -c -g -package labltk gui.ml

clean:
	rm *.cmi *.cmo run_tests tbui gui
