# common/Makefile

OCAMLC   = ocamlc
OCAMLDEP = ocamldep

UTILS_DIR = ../utils
INCLUDES  = -I . -I $(UTILS_DIR)

NAME     = Common
LIB      = $(NAME).cma
PACK_OBJ = $(NAME).cmo

# Tous les .ml de common (donc: adder.ml + eval_formula.ml + ... etc)
ML_FILES   = $(wildcard *.ml)
ORDERED_ML = $(shell $(OCAMLDEP) $(INCLUDES) -sort $(ML_FILES))
CMO_FILES  = $(ORDERED_ML:.ml=.cmo)

.PHONY: all common utils clean fclean re

all: utils $(LIB)
common: all

utils:
	$(MAKE) -C $(UTILS_DIR)

# Compile dans l'ordre ocamldep
%.cmo: %.ml
	$(OCAMLC) $(INCLUDES) -c $<

$(PACK_OBJ): $(CMO_FILES)
	$(OCAMLC) -pack -o $(PACK_OBJ) $(CMO_FILES)

$(LIB): $(PACK_OBJ)
	$(OCAMLC) -a -o $(LIB) $(PACK_OBJ)

clean:
	rm -f *.cmo *.cmi

# SUPER IMPORTANT pour éviter "inconsistent assumptions"
# => on force un rebuild propre de la lib
fclean: clean
	rm -f $(LIB) $(PACK_OBJ) $(NAME).cmi

re: fclean all
