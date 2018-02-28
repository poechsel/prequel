all:
	ocamlbuild -yaccflag -v -lib unix src/main.native; 
byte:
	ocamlbuild -yaccflag -v src/main.byte
test:
	ocamlbuild -package oUnit -Is src/ tests/test.byte -r
clean:
	ocamlbuild -clean

