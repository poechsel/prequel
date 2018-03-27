all:
	ocamlbuild -use-ocamlfind\
		-package str\
		-package csv\
		-package ppx_deriving.std\
		-package fpath\
		-lib unix\
		-cflag -unsafe\
		src/main.native;
test:
	ocamlbuild -use-ocamlfind\
		-package oUnit\
		-package ppx_deriving.std\
		-package fpath\
		-package csv\
		-Is src/\
		tests/tests.byte -r;
	./tests.byte
clean:
	ocamlbuild -clean

