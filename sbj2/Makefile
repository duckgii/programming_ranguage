CC = c++
NAME = harlFilter
CFLAGS = -std=c++98 -Wall -Wextra -Werror
SRCS = lex.l \
        hw3.y
SRCS_OBJS = lex.yy.c \
            y.tab.c

all : $(NAME)

clean :
    rm -f $(SRCS_OBJS)

fclean : clean
    rm -f $(NAME)

re:
    make fclean
    make all

$(NAME) : $(SRCS)
	lex lex.l 
	yacc –d hw3.y
	cc y.tab.c lex.yy.c –o yacc

.PHONY : all clean fclean re