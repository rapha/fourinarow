SOURCES=player.ml board.ml game.ml ai.ml
OBJECTS=player.cmx board.cmx game.cmx ai.cmx

all: run_tests tbui gui

run_tests: tests
	./run_tests

gui: $(OBJECTS) gui.cmx
	ocamlfind batteries/ocamlopt -o gui -g -linkpkg $(OBJECTS) -package labltk gui.cmx

tbui: $(OBJECTS) tbui.cmx
	ocamlfind batteries/ocamlopt -o tbui -g -linkpkg $(OBJECTS) tbui.cmx

tests: $(OBJECTS) test.cmx
	ocamlfind batteries/ocamlopt -o run_tests -g -linkpkg -package oUnit $(OBJECTS) test.cmx

$(OBJECTS) : $(SOURCES)
	ocamlfind batteries/ocamlopt -c -g $(SOURCES)

test.cmx: $(OBJECTS) test.ml
	ocamlfind batteries/ocamlopt -c -g -package oUnit test.ml

tbui.cmx: tbui.ml
	ocamlfind batteries/ocamlopt -c -g tbui.ml

gui.cmx: gui.ml
	ocamlfind batteries/ocamlopt -c -g -package labltk gui.ml

clean:
	rm *.cmi *.cmo *.cmx *.o run_tests tbui gui
