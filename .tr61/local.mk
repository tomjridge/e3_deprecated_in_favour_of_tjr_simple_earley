MLS:=$(filter-out test.ml interactive.ml, $(shell cd src && ocamlfind ocamldep -sort *.ml))
mods_for_lib:
	echo $(MLS) | .tr61/mods_for_lib.scala

order:
	echo $(MLS)

order_cmi:
	echo $(MLS) | sed -e 's/.ml/.cmi/g'
