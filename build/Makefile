SHELL:=bash

CAMLCINCLUDES:=str.cma
CAMLOPTINCLUDES:=str.cmxa

MLS:=$(filter-out interactive.ml, $(shell ocamlfind ocamldep -sort *.ml *.mli))
SRC_DIR:=../src

MLS_TO_EXEC:=e3_examples.ml e3_test.ml # executables
IGNORE_FOR_LIB:=$(MLS_TO_EXEC) interactive.ml # don't want these in the lib
MLS_FOR_LIB:=$(filter-out $(IGNORE_FOR_LIB), $(MLS))
LIB:=e3


all: before_all
	$(MAKE) cmo cmx 
	$(MAKE) $(LIB).cma $(LIB).cmxa $(MLS_TO_EXEC:.ml=.native)


include Makefile.include


clean: before_clean

