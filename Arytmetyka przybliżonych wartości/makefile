all:
	ocamlc -o arytmetyka.out arytmetyka.mli arytmetyka.ml
	./arytmetyka.out

arytmetyka.cmi: arytmetyka.mli
	ocamlc -c arytmetyka.mli

arytmetyka.cmo: arytmetyka.ml arytmetyka.cmi
	ocamlc -c arytmetyka.ml

clean:
	rm *.cmi *.cmo *.out
