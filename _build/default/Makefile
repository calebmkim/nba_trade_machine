.PHONY: test check

build:
	dune build
zip: 
	zip -r src.zip . -x _build/\* .git/\*
test:
	OCAMLRUNPARAM=b dune exec test/main.exe
play:
	OCAMLRUNPARAM=b dune exec state_handler/play.exe