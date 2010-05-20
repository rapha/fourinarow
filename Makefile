all: test

test: 
	ocamlbuild -cflags -package,oUnit -lflags -package,oUnit,-thread test.native
	./test.native

tui: test
	ocamlbuild -lflags -thread tui.native

gui: test
	ocamlbuild -cflags -I,+labltk -lflags -I,+labltk,-thread -lib labltk gui.native

clean:
	ocamlbuild -clean

