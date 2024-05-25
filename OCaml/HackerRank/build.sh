ocamlopt -o $1 $1.ml

./$1 

rm $1 $1.cmx $1.o $1.cmi
