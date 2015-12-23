SHELL:=bash

all:
	ocamlbuild -I src e3_test.native e3_examples.native 
	ocamlbuild -I src e3.cma e3.cmxa
#cd build && $(MAKE)

install: all
	ocamlfind install e3 META _build/src/*.cmi  _build/src/e3.cma _build/src/e3.cmxa _build/src/e3.a

test: all
	./_build/src/e3_examples.native
	./_build/src/e3_test.native

clean:
	ocamlbuild -clean
# cd build && $(MAKE) clean


-include Makefile.local
