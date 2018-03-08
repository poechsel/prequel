all:
	ocamlbuild -yaccflag -v -package str -package csv -lib unix src/main.native; 
byte:
	ocamlbuild -yaccflag -v -package csv src/main.byte
test:
	ocamlbuild -package oUnit -package csv -Is src/ tests/test.byte -r ;
	./test.byte
clean:
	ocamlbuild -clean

