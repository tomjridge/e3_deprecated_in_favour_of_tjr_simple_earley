SHELL=bash

CAMLC=$(shell command -v ocamlc.opt || command -v ocamlc || echo missing_ocamlc)
CAMLOPT=$(shell command -v ocamlopt.opt || command -v ocamlopt || echo missing_ocamlopt)
CAMLDEP=$(shell command -v ocamldep.opt || command -v ocamldep || echo missing_ocamldep) -one-line
OCAMLDOC=$(shell command -v ocamldoc.opt || command -v ocamldoc || echo missing_ocamldoc)

WARNINGS=#
COMPFLAGS=$(WARNINGS)

E3=e3_core.cmo
CMO=$(E3) 

NATIVES=# 

all: e3.cma e3.cmxa

e3.cma: $(CMO)
	$(CAMLC) $(CAMLCINCLUDES) -a -o $@ $(CMO)

e3.cmxa: $(CMO:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $(CMO:.cmo=.cmx)

$(NATIVES): %.native: %.ml p3.cmxa
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -o $@ p3.cmxa $<


# Default rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(CAMLCINCLUDES) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(CAMLCINCLUDES) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(FORPACK) $(COMPFLAGS) $(CAMLOPTINCLUDES) -c $<


doc: $(P3)
	$(OCAMLDOC) -html $(P3:.cmo=.ml)


depend:
	$(CAMLDEP) $(INCLUDES) *.mli *.ml >.depend

include .depend


clean:
	rm -f $(CMO) $(CMO:.cmo=.cmi) $(CMO:.cmo=.cmx) $(NATIVES:.native=.cmo) $(NATIVES:.native=.cmi) $(NATIVES:.native=.cmx) {minip3,mini_p3_lib}.cm[iox]
	rm -f *.cma *.cmxa *.a $(CMO:.cmo=.o) *.native $(NATIVES:.native=.o) {minip3,mini_p3_lib}.o
	rm -f *.{html,css}

realclean:
	rm -f p3_$(VERSION).{o,cmi,cmo,cmx}
