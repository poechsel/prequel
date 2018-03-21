all:
	ocamlbuild -yaccflag -v -package str -package csv -lib unix -cflag -unsafe src/main.native; 
byte:
	ocamlbuild -yaccflag -v -package csv src/main.byte
test:
	ocamlbuild -package oUnit -package csv -Is src/ tests/tests.byte -r ;
	./tests.byte
clean:
	ocamlbuild -clean

