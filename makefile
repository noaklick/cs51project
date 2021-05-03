all: miniml evaluation expr tests

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

tests: tests.ml
	ocamlbuild -use-ocamlfind tests.byte

clean:
	rm -rf _build *.byte
