all:
	ocamlbuild camelcrap.byte
clean:
	ocamlbuild -clean

.PHONY: clean
