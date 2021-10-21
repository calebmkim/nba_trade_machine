build:
	dune build
zip: 
	zip -r project.zip . -x _build/\* .git/\*
play:
	OCAMLRUNPARAM=b dune exec state_handler/play.exe