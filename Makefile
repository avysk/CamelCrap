all:
	ocamlbuild camelcrap.byte -libs graphics
clean:
	ocamlbuild -clean

.PHONY: clean
