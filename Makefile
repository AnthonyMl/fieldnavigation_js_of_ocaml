OCAMLBUILD := ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)"

SRCDIR     := src
OCAMLSRC   := $(wildcard $(SRCDIR)/*.ml)
SRC        := $(OCAMLSRC:%.ml=%.js)

all:
	$(OCAMLBUILD) $(SRC)

.PHONY: all
