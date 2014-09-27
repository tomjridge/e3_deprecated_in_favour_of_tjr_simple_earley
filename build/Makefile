SHELL=bash

CAMLC=$(shell command -v ocamlc.opt || command -v ocamlc || echo missing_ocamlc)
CAMLOPT=$(shell command -v ocamlopt.opt || command -v ocamlopt || echo missing_ocamlopt)
CAMLDEP=$(shell command -v ocamldep.opt || command -v ocamldep || echo missing_ocamldep) -one-line
OCAMLDOC=$(shell command -v ocamldoc.opt || command -v ocamldoc || echo missing_ocamldoc)

# FIXME remove this suppresion of warnings
WARNINGS=-w -8#
COMPFLAGS=$(WARNINGS)

E3=e3_core.cmo e3_simple.cmo e3_array.cmo
CMO=$(E3) 

NATIVES=e3_examples.native e3_test.native

all: e3.cma e3.cmxa $(NATIVES)

e3.cma: $(CMO)
	$(CAMLC) $(CAMLCINCLUDES) -a -o $@ $(CMO)

e3.cmxa: $(CMO:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $(CMO:.cmo=.cmx)

$(NATIVES): %.native: %.ml e3.cmxa
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -o $@ e3.cmxa $<


# Default rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(CAMLCINCLUDES) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(CAMLCINCLUDES) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(FORPACK) $(COMPFLAGS) $(CAMLOPTINCLUDES) -c $<


doc: e3_core.mli e3_simple.mli
	$(OCAMLDOC) -html e3_core.mli e3_simple.mli


depend:
	$(CAMLDEP) $(INCLUDES) *.mli *.ml >.depend

include .depend


clean:
	rm -f $(CMO) $(CMO:.cmo=.cmi) $(CMO:.cmo=.cmx) $(NATIVES:.native=.cmo) $(NATIVES:.native=.cmi) $(NATIVES:.native=.cmx) 
	rm -f *.cma *.cmxa *.a $(CMO:.cmo=.o) *.native $(NATIVES:.native=.o) 
	rm -f *.{html,css}

realclean:
