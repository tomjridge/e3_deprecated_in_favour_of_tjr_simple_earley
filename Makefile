SHELL:=bash

OB:=ocamlbuild -I src -cflag -w -cflag -8

all:
	$(OB) core_types.cmo map_set_types.cmo core.cmo simple.cmo test.cmo hashtbl_impl.cmo test.native 

cmi:
	$(OB) symbol.cmi substring.cmi

#	$(OB) src e3_test.native e3_examples.native 
#	$(OB) src e3.cma e3.cmxa

#cd build && $(MAKE)

install: all
	ocamlfind install e3 META _build/src/*.cmi  _build/src/e3.cma _build/src/e3.cmxa _build/src/e3.a

test: all
	./test.native

doc:
	$(OB) e3.docdir/index.html

clean:
	$(OB) -clean
# cd build && $(MAKE) clean


-include local.mk
