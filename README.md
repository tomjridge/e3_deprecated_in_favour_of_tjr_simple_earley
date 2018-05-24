# NOTE!!! this repository is deprecated in favour of tjr_simple_earley


# OCaml Earley parser

Requires very new OCaml: <https://github.com/ocaml/ocaml/commit/30bb2c39d8509dc741c0321c700b512820059eb3>

To get:

~~~
git clone ...
cd e3
~~~


To install:

~~~
make
make install
~~~


Alternatively via nix:

~~~
nix-build
~~~

The result will be in `./result`


To build API-doc:

~~~
make doc
~~~

Result will be in `./simple_doc`
