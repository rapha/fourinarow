all: spec

clean:
	rm -f *.cm* *.byte *.a *.o *.exe .source_dependencies

game.cma: piece.cmo row.cmo col.cmo columns.cmo line.cmo lines.cmo board.cmo player.cmo ai.cmo game.cmo
	$(OCAMLC) -a -package batteries piece.cmo row.cmo col.cmo columns.cmo line.cmo lines.cmo board.cmo player.cmo ai.cmo game.cmo -o game.cma

game.cmxa: piece.cmx row.cmx col.cmx columns.cmx line.cmx lines.cmx board.cmx player.cmx ai.cmx game.cmx
	$(OCAMLOPT) -a -package batteries piece.cmx row.cmx col.cmx columns.cmx line.cmx lines.cmx board.cmx player.cmx ai.cmx game.cmx -o game.cmxa

spec: game.cma spec.ml
	ospecl spec.ml

tui.byte: game.cma tui.ml
	$(OCAMLC) -thread -linkpkg game.cma tui.ml -o tui.byte

tui.exe: game.cmxa tui.ml
	$(OCAMLOPT) -thread -linkpkg game.cmxa tui.ml -o tui.exe

gui.byte: game.cma gui.ml
	$(OCAMLC) -thread -package labltk -linkpkg game.cma gui.ml -o gui.byte

.PHONY: all clean spec

OCAMLC = ocamlfind ocamlc -g -warn-error A -package batteries
OCAMLOPT = ocamlfind ocamlopt -g -warn-error A -package batteries

# simple file transforms
.SUFFIXES: .mli .ml .cmi .cmo .cmx
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmo:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

# autogenerate source dependencies
.source_dependencies: *.mli *.ml
	ocamldep *.mli *.ml >.source_dependencies
include .source_dependencies
