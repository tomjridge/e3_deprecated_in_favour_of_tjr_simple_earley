SHELL:=bash

OB:=ocamlbuild -I src

all:
	$(OB) core_types.cmo map_set_types.cm 

cmi:
	$(OB) symbol.cmi substring.cmi

#	$(OB) src e3_test.native e3_examples.native 
#	$(OB) src e3.cma e3.cmxa

#cd build && $(MAKE)

install: all
	ocamlfind install e3 META _build/src/*.cmi  _build/src/e3.cma _build/src/e3.cmxa _build/src/e3.a

test: all
	./_build/src/e3_examples.native
	./_build/src/e3_test.native

clean:
	$(OB) -clean
# cd build && $(MAKE) clean


-include local.mk
