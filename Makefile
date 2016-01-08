SHELL:=bash

default: all

SRC_LINKED:=src.linked

-include local.mk

$(SRC_LINKED): 
	-rm -rf $@
	mkdir $@
	cd $@ && find ../src -type f -exec ln -s \{\} . \;
	ln -s ../.tr61/interactive.ml $@


# OB_IS:=-Is src,src/core,src/impl,src/sets_maps,src/test
OB:=ocamlbuild -I $(SRC_LINKED) -cflag -w -cflag -8

all: $(SRC_LINKED)
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
	rm -rf $(SRC_LINKED)

FORCE:


