# utils/Makefile

OCAMLC    = ocamlc
OCAMLDEP  = ocamldep

NAME      = Utils
LIB       = $(NAME).cma
PACK_OBJ  = $(NAME).cmo

ML_FILES     = $(wildcard *.ml)
ORDERED_ML   = $(shell $(OCAMLDEP) -sort $(ML_FILES))
CMO_FILES    = $(ORDERED_ML:.ml=.cmo)

.PHONY: all utils clean fclean re

all: $(LIB)
utils: all

%.cmo: %.ml
	$(OCAMLC) -c $<

$(PACK_OBJ): $(CMO_FILES)
	$(OCAMLC) -pack -o $(PACK_OBJ) $(CMO_FILES)

$(LIB): $(PACK_OBJ)
	$(OCAMLC) -a -o $(LIB) $(PACK_OBJ)

clean:
	rm -f *.cmo *.cmi

fclean: clean
	rm -f $(LIB) $(PACK_OBJ) $(NAME).cmi

re: fclean all
