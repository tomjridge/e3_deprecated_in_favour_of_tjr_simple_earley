SHELL:=bash

default: all

SRC_LINKED:=src.linked

-include local.mk

# need to rebuild on addition/removal of files
$(SRC_LINKED): 
	-rm -rf $@
	mkdir $@
	cd $@ && find ../src -type f -exec ln -s \{\} . \;
	ln -s ../.tr61/interactive.ml $@

# OB_IS:=-Is src,src/core,src/impl,src/sets_maps,src/test
OB:=ocamlbuild -I $(SRC_LINKED) -cflag -w -cflag -8

OD:=ocamlfind ocamldoc

all: test.native examples.native e3.cma e3.cmxa 

%.cma: $(SRC_LINKED)
	$(OB) $@

%.cmxa: $(SRC_LINKED)
	$(OB) $@

%.native: $(SRC_LINKED)
	$(OB) $@


cmi: _build/cmi

_build/cmi:
	$(OB) hashed_sets_and_maps.cmi set_map_types.cmi \
	  core_types.cmi core.cmi common_impl.cmi hashtbl_impl.cmi simple_impl.cmi
	touch $@


install: all
	ocamlfind install e3 META \
	  _build/src/*.cmi  _build/src/e3.cma _build/src/e3.cmxa _build/src/e3.a


test: test.native
	./test.native


doc: simple_doc


# FIXME this doesn't seem to work
simple_doc: test.native # to force compile
	-mkdir $@
	$(OD) -html -I _build/$(SRC_LINKED) -d $@ $(SRC_LINKED)/impl_t.mli $(SRC_LINKED)/hashtbl_impl.mli \
	  $(SRC_LINKED)/simple_impl.mli

# various errors, not clear why
e3.docdir:  test.native # to force compile
	$(OB) e3.docdir/index.html


clean:
	$(OB) -clean
	rm -rf $(SRC_LINKED) simple_doc

FORCE:


