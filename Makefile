all:
	cd build && make
	./build/e3_examples.native
	./build/e3_test.native

clean:
	cd build && make clean
