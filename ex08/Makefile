OCAMLC   = ocamlc
OCAMLDEP = ocamldep

UTILS_DIR  = ../utils
COMMON_DIR = ../common
INCLUDES   = -I . -I $(UTILS_DIR) -I $(COMMON_DIR)

NAME = main

ML_FILES   = $(wildcard *.ml)
ORDERED_ML = $(shell $(OCAMLDEP) $(INCLUDES) -sort $(ML_FILES))
CMO_FILES  = $(ORDERED_ML:.ml=.cmo)

.PHONY: all utils common clean fclean re run

all: utils common $(NAME)

utils:
	$(MAKE) -C $(UTILS_DIR)

common:
	$(MAKE) -C $(COMMON_DIR)

$(NAME): $(CMO_FILES)
	$(OCAMLC) $(INCLUDES) -o $(NAME) \
	  $(UTILS_DIR)/Utils.cma \
	  $(COMMON_DIR)/Common.cma \
	  $(CMO_FILES)

%.cmo: %.ml
	$(OCAMLC) $(INCLUDES) -c $<

clean:
	rm -f *.cmo *.cmi

fclean: clean
	rm -f $(NAME)

re: fclean all

run: all
	./$(NAME)
