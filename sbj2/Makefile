# --- 변수 선언 ---
NAME      = yacc
CC        = cc

SRC_LEX   = hw3.l
SRC_YACC  = hw3.y

# --- 생성 산출물 정의 ---
GEN_C     = lex.yy.c y.tab.c
GEN_HDR   = y.tab.h
OBJS      = lex.yy.o y.tab.o

# --- 기본 타겟 ---
all: $(NAME)

# --- 링크 단계: 오브젝트 → 실행파일 ---
$(NAME): $(OBJS)
	$(CC) -o $@ $(OBJS)

# --- Bison으로 파서 소스·헤더 생성 ---
y.tab.c y.tab.h: $(SRC_YACC)
	yacc -d -v $(SRC_YACC)

# --- Lex로 렉서 소스 생성 ---
lex.yy.c: $(SRC_LEX) y.tab.h
	lex $(SRC_LEX)

# --- C 소스 → 오브젝트 컴파일 ---
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# --- 정리 ---
clean:
	rm -f $(GEN_C) $(OBJS) $(GEN_HDR)

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re