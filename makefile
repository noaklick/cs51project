all: miniml evaluation expr

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte


clean:
	rm -rf _build *.byte
