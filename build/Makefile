SHELL=bash

SRC_DIR:=../src
MLS_TO_EXEC:=e3_examples.ml e3_test.ml # executables
IGNORE_FOR_LIB:=$(MLS_TO_EXEC) interactive.ml # don't want these in the lib
MLS_FOR_LIB_1=$(shell ocamlfind ocamldep -sort *.ml)
MLS_FOR_LIB=$(filter-out $(IGNORE_FOR_LIB), $(MLS_FOR_LIB_1)) # p1_examples.ml is an executable
CMO=$(MLS_FOR_LIB:.ml=.cmo)

LIB:=e3

# FIXME remove this suppresion of warnings
WARNINGS:=-w -8#
COMPFLAGS:=$(WARNINGS)

all: before_all
	make $(CMO) e3.cma e3.cmxa e3_examples.native e3_test.native

include Makefile.include

clean:
	rm -f *.ml *.mli *.cmo *.cmi *.cmx *.native *.a *.o *.cma *.cmxa .depend
	rm -f *.{html,css}

