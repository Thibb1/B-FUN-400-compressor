##
## EPITECH PROJECT, 2022
## Makefile
## File description:
## Makefile
##

OUT	= imageCompressor

all: $(OUT)

$(OUT):
	stack install --local-bin-path .
	mv compressor-exe $(OUT)

clean:
	stack clean

fclean: clean
	$(RM) $(OUT)
	stack purge

re: fclean all