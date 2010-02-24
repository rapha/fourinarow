all: test tui gui

test: 
	ocamlfind batteries/ocamlbuild -cflags -package,oUnit -lflags -package,oUnit test.native
	./test.native

tui:
	ocamlfind batteries/ocamlbuild tui.p.native

gui: 
	ocamlfind batteries/ocamlbuild -cflags -package,labltk -lflags -package,labltk gui.native

clean:
	ocamlbuild -clean

