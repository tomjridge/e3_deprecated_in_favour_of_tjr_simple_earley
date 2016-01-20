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

FORCE:
