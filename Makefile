all: spec

clean:
	find -E . -regex '.*\.(cm..?|a|o|exe|byte|source_dependencies)' | xargs rm

game.cma: src/piece.cmo src/row.cmo src/col.cmo src/columns.cmo src/line.cmo src/lines.cmo src/board.cmo src/player.cmo src/ai.cmo src/game.cmo
	$(OCAMLC) -a -package batteries src/piece.cmo src/row.cmo src/col.cmo src/columns.cmo src/line.cmo src/lines.cmo src/board.cmo src/player.cmo src/ai.cmo src/game.cmo -o game.cma

game.cmxa: src/piece.cmx src/row.cmx src/col.cmx src/columns.cmx src/line.cmx src/lines.cmx src/board.cmx src/player.cmx src/ai.cmx src/game.cmx
	$(OCAMLOPT) -a -package batteries src/piece.cmx src/row.cmx src/col.cmx src/columns.cmx src/line.cmx src/lines.cmx src/board.cmx src/player.cmx src/ai.cmx src/game.cmx -o game.cmxa

spec: game.cma spec/spec.ml
	ospecl -I src spec/spec.ml

tui.byte: game.cma src/tui.ml
	$(OCAMLC) -thread -linkpkg game.cma src/tui.ml -o tui.byte

tui.exe: game.cmxa src/tui.ml
	$(OCAMLOPT) -thread -linkpkg game.cmxa src/tui.ml -o tui.exe

gui.byte: game.cma gui.ml
	$(OCAMLC) -thread -package labltk -linkpkg game.cma src/gui.ml -o gui.byte

.PHONY: all clean spec

OCAMLC = ocamlfind ocamlc -g -warn-error A -package batteries
OCAMLOPT = ocamlfind ocamlopt -g -warn-error A -package batteries

# simple file transforms
.SUFFIXES: .mli .ml .cmi .cmo .cmx
.mli.cmi:
	$(OCAMLC) -c -I src -I spec $<
.ml.cmo:
	$(OCAMLC) -c -I src -I spec $<
.ml.cmx:
	$(OCAMLOPT) -c -I src -I spec $<

# autogenerate source dependencies
.source_dependencies: src/*.mli src/*.ml
	ocamldep src/*.mli src/*.ml >.source_dependencies
include .source_dependencies
