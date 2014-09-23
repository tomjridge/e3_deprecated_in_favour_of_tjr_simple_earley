all:
	cd build && make
	./build/e3_examples.native

clean:
	cd build && make clean
