############
OCAMLC=/usr/bin/env ocamlc
OCAMLLEX=/usr/bin/env ocamllex
OCAMLYACC=/usr/bin/env ocamlyacc
OCAMLC_FLAGS=



############
all: lexer parser

lexer:
	$(OCAMLLEX) lexer.mll

parser:
	$(OCAMLYACC) parser.mly
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c parser.ml
	$(OCAMLC) -c pokemon.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -o pokemon lexer.cmo parser.cmo pokemon.cmo


############
clean:
	rm -f parser.cmo
	rm -f parser.ml
	rm -f parser.mli
	rm -f parser.cmi

	rm -f pokemon.cmo
	rm -f pokemon
	rm -f pokemon.cmi

	rm -f lexer.ml
	rm -f lexer.cmo
	rm -f lexer.cmi
