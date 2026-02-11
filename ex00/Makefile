# ex00/Makefile

OCAMLC    = ocamlc
UTILS_DIR = ../utils
INCLUDES  = -I $(UTILS_DIR)

NAME = main

SRC = adder.ml main.ml
CMO = $(SRC:.ml=.cmo)
CMI = $(SRC:.ml=.cmi)

.PHONY: all clean fclean re run utils

all: utils $(NAME)

# Construit la lib Utils.cma dans ../utils
utils:
	$(MAKE) -C $(UTILS_DIR)

$(NAME): $(CMO) utils
	$(OCAMLC) $(INCLUDES) -o $(NAME) $(UTILS_DIR)/Utils.cma $(CMO)

adder.cmo: adder.ml
	$(OCAMLC) -c $<

main.cmo: main.ml
	$(OCAMLC) $(INCLUDES) -c $<

clean:
	rm -f $(CMO) $(CMI)

fclean: clean
	rm -f $(NAME)

re: fclean all

run: all
	./$(NAME)
