all: miniml evaluation expr tests free_vars_tests

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

tests: tests.ml
	ocamlbuild -use-ocamlfind tests.byte

free_vars_tests: free_vars_tests.ml
	ocamlbuild -use-ocamlfind free_vars_tests.byte


clean:
	rm -rf _build *.byte
