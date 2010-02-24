all: test

test: 
	ocamlfind batteries/ocamlbuild -cflags -package,oUnit -lflags -package,oUnit test.native
	./test.native

tui: test
	ocamlfind batteries/ocamlbuild tui.p.native

gui: test
	ocamlfind batteries/ocamlbuild -cflags -package,labltk -lflags -package,labltk gui.native

clean:
	ocamlbuild -clean

