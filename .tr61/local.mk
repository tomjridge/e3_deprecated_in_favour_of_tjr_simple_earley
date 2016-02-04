OCAMLC:=/tmp/l/nix-profile/bin/ocamlc
OCAMLOPT:=/tmp/l/nix-profile/bin/ocamlopt
OB:=ocamlbuild -ocamlc $(OCAMLC) -ocamlopt $(OCAMLOPT) -I $(SRC_LINKED) -cflag -w -cflag -8

# most targets below need src.linked
MLS:=$(filter-out examples.ml test.ml interactive.ml, $(shell cd $(SRC_LINKED) && ocamlfind ocamldep -sort *.ml))
MLIS:=$(filter-out test.mli, $(shell cd $(SRC_LINKED) && ocamlfind ocamldep -sort *.mli))

mods_for_lib:
	echo $(MLS) | .tr61/mods_for_lib.scala

mods_for_int:
	echo $(MLS) | .tr61/mods_for_int.scala

order:
	echo $(MLS)

order_cmi:
	echo $(MLIS) 

# doesn't work 
dep_with_dirs:
	ocamlfind ocamldep `find src -name "*.ml" -or -name "*.mli"`


depend:
	cd $(SRC_LINKED) &&  ocamlfind ocamldep *.mli *.ml


ED:=/tmp/l/Dropbox/www_resources/e3/e3.docdir
export: e3.docdir
	-rm -rf $(ED)/e3.docdir
	cp -R $< $(ED)

FORCE:


# FIXME what was this supposed to do? do a minimal build? 
cmi: _build/cmi

_build/cmi: $(SRC_LINKED)
	$(OB) impl_t.cmi hashtbl_impl.cmi simple_impl.cmi
	touch $@

