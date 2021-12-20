.PHONY: test check

build:
	dune build
zip: 
	zip -r src.zip . -x _build/\* .git/\*
test:
	OCAMLRUNPARAM=b dune exec test/main.exe
per_model: 
	OCAMLRUNPARAM=b dune exec model_build/per_model.exe
play:
	OCAMLRUNPARAM=b dune exec state_handler/play.exe
docs:
	dune build @doc