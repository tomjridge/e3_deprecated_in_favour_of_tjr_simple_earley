SHELL:=bash

default: all

SRC_LINKED:=src.linked
BS:=_build/$(SRC_LINKED)

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

all: e3.cma e3.cmxa test.native examples.native 

%.cma: $(SRC_LINKED)
	$(OB) $@

%.cmxa: $(SRC_LINKED)
	$(OB) $@

%.native: $(SRC_LINKED)
	$(OB) $@


install: all
	ocamlfind install e3 META $(BS)/*.cmi $(BS)/e3.cma $(BS)/e3.cmxa # _build/src/e3.a


test: test.native
	./test.native


doc: simple_doc


simple_doc: test.native # to force compile
	-mkdir $@
	$(OD) -html -I _build/$(SRC_LINKED) -d $@ \
	  $(SRC_LINKED)/impl_t.mli $(SRC_LINKED)/hashtbl_impl.mli $(SRC_LINKED)/simple_impl.mli

# various errors, not clear why
e3.docdir:  test.native # to force compile
	$(OB) e3.docdir/index.html


clean:
	$(OB) -clean
	-rm -rf $(SRC_LINKED) simple_doc
	-rm result

FORCE:


