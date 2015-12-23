SHELL:=bash

OB:=ocamlbuild

all:
	$(OB) -I src symbol.cmo


#	$(OB) -I src e3_test.native e3_examples.native 
#	$(OB) -I src e3.cma e3.cmxa

#cd build && $(MAKE)

install: all
	ocamlfind install e3 META _build/src/*.cmi  _build/src/e3.cma _build/src/e3.cmxa _build/src/e3.a

test: all
	./_build/src/e3_examples.native
	./_build/src/e3_test.native

clean:
	$(OB) -clean
# cd build && $(MAKE) clean


-include Makefile.local
