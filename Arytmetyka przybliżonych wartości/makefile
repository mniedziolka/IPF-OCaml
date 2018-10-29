all:
	ocamlc -o test arytmetyka.mli arytmetyka.ml

arytmetyka.cmi: arytmetyka.mli
	ocamlc -c arytmetyka.mli

arytmetyka.cmo: arytmetyka.ml arytmetyka.cmi
	ocamlc -c arytmetyka.ml

test: tests.ml arytmetyka.mli arytmetyka.ml
	ocamlc -o test.out arytmetyka.mli arytmetyka.ml tests.ml
	./test.out
test_abramowicz: tests.ml arytmetyka.mli arytmetyka_abramowicz.ml
	ocamlc -o test_abramowicz.out arytmetyka.mli arytmetyka_abramowicz.ml tests.ml
	./test_abramowicz.out
clean:
	rm *.cmi *.cmo *.out
