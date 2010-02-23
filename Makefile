all: test tbui gui

test: 
	ocamlfind batteries/ocamlbuild -cflags -package,oUnit -lflags -package,oUnit test.native
	./test.native

tbui: 
	ocamlfind batteries/ocamlbuild tbui.p.native

gui: 
	ocamlfind batteries/ocamlbuild -cflags -package,labltk -lflags -package,labltk gui.native

clean:
	ocamlbuild -clean

