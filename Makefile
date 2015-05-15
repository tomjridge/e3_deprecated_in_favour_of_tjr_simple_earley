all:
	cd build && $(MAKE)

install: all
	ocamlfind install e3 META build/*.cmi  build/e3.cma build/e3.cmxa build/e3.a

test: all
	./build/e3_examples.native
	./build/e3_test.native

clean:
	cd build && $(MAKE) clean
