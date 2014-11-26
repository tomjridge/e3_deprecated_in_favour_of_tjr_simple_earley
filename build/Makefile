SHELL=bash

SRC_DIR:=../src
MLS_TO_EXEC:=e3_examples.ml e3_test.ml # executables
IGNORE_FOR_LIB:=$(MLS_TO_EXEC) interactive.ml # don't want these in the lib
MLS_FOR_LIB:=$(shell ocamlfind ocamldep -sort *.ml)
MLS_FOR_LIB:=$(filter-out $(IGNORE_FOR_LIB), $(MLS_FOR_LIB)) # p1_examples.ml is an executable
CMO:=$(MLS_FOR_LIB:.ml=.cmo)

LIB:=e3

# FIXME remove this suppresion of warnings
WARNINGS:=-w -8#
COMPFLAGS:=$(WARNINGS)

all:
	make before_all
	make $(CMO) e3.cma e3.cmxa e3_examples.native e3_test.native

include Makefile.include

clean:
	rm -f *.ml *.mli
	rm -f $(CMO) $(CMO:.cmo=.cmi) $(CMO:.cmo=.cmx) $(MLS_TO_EXEC:.ml=.cmo) $(MLS_TO_EXEC:.ml=.cmi) $(MLS_TO_EXEC:.native=.cmx) 
	rm -f *.cmx *.cma *.cmxa *.a $(CMO:.cmo=.o) *.native $(MLS_TO_EXEC:.ml=.o) 
	rm -f *.{html,css}

