SHELL:=bash

OB:=ocamlbuild -I src -cflag -w -cflag -8

all:
	$(OB) core_types.cmo core.cmo simple_impl.cmo hashtbl_impl.cmo test.native examples.native
	$(OB) e3.cma e3.cmxa

cmi:
	$(OB) hashed_sets_and_maps.cmi set_map_types.cmi \
	  core_types.cmi core.cmi common_impl.cmi hashtbl_impl.cmi simple_impl.cmi

#	$(OB) src e3_test.native e3_examples.native 

install: all
	ocamlfind install e3 META \
	  _build/src/*.cmi  _build/src/e3.cma _build/src/e3.cmxa _build/src/e3.a

test: all
	./test.native

doc:
	$(OB) e3.docdir/index.html

clean:
	$(OB) -clean


-include local.mk
