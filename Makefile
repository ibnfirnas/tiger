build:
	@ocamlbuild -use-menhir src/tiger.native


clean:
	@ocamlbuild -clean
