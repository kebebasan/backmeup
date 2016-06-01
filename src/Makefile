mlsrc = backmeup.ml random_split.ml

all : backmeup random_split

backmeup : backmeup.ml 
	ocamlfind ocamlc -g -package labltk -linkpkg unix.cma str.cma backmeup.ml -o backmeup
random_split : random_split.ml
	ocamlfind ocamlc unix.cma str.cma random_split.ml -o random_split
clean :
	rm -f *.cmi
	rm -f *.cmo
install:
	cp backmeup ~/bin
	cp random_split ~/bin

