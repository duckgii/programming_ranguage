
D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*
ID    		[A-Za-z_][A-Za-z0-9_]*

%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "y.tab.h"

void count();
void comment();
void lineComment();
int check_type();
int is_typedef_name(const char *s);
int typedef_start_flag; // typedef의 시작과 끝을 확인하기 위한 flag입니다.
int curly_brace_flag; // 구조체의 시작과 끝을 확인하기 위한 flag입니다.
/* 심볼 테이블 최대 개수 */
#define MAX_SYMBOLS 1024

// 사용자 지정 변수, 자료형을 따로 저장해두기 위해 심볼테이블을 활용했습니다.
/* typedef 이름 테이블 */
static char *typedef_names[MAX_SYMBOLS];
static int   typedef_count = 0;
/* #define 이름 테이블 */
static char *define_names[MAX_SYMBOLS];
static int   define_count = 0;
 

/* typedef 이름 추가 */
void add_typedef(const char *s) {
    if (typedef_count < MAX_SYMBOLS)
        typedef_names[typedef_count++] = strdup(s);
}

/* typedef 여부 확인 */
int is_typedef(const char *s) {
    for (int i = 0; i < typedef_count; i++)
        if (strcmp(s, typedef_names[i]) == 0)
            return 1;
    return 0;
}

/* #define 이름 추가 */
void add_define(const char *s) {
    if (define_count < MAX_SYMBOLS)
        define_names[define_count++] = strdup(s);
}

/* #define 여부 확인 */
int is_define(const char *s) {
    for (int i = 0; i < define_count; i++)
        if (strcmp(s, define_names[i]) == 0)
            return 1;
    return 0;
}

// typedef 시작할 때 flag 초기화 함수
void start_typedef(void)
{
	typedef_start_flag = 1; 
	curly_brace_flag = -1; 
}
%}


%%
"/*"			{ comment(); }  // 여러줄 주석부분 무시를 위한 함수 호출후 토큰 반환하지 않음
"//"			{ lineComment(); } // 한줄 주석 무시를 위한 함수 호출후 토큰 반환하지 않음
^"#include".*\n    { ; } // #include 로 헤더를 호출하는 부분은 개행까지 무시하고 토큰은 반환하지 않는 방법으로 syntax에러를 방지했습니다.
^"#define"[ \t]+{ID}.*\n {
	char namebuf[128];
    sscanf(yytext, "#%*s %127s", namebuf);
    add_define(namebuf);
	/*
		define으로 전처리 선언된 변수의 이름을 저장하여 사용자 지정 type으로 저장합니다.
	*/
}
"typedef"		{ count(); start_typedef(); return(TYPEDEF);}
"auto"			{ count(); return(AUTO); }
"break"			{ count(); return(BREAK); }
"case"			{ count(); return(CASE); }
"char"			{ count(); return(CHAR); }
"const"			{ count(); return(CONST); }
"continue"		{ count(); return(CONTINUE); }
"default"		{ count(); return(DEFAULT); }
"do"			{ count(); return(DO); }
"double"		{ count(); return(DOUBLE); }
"else if"			{ count(); return(ELSEIF); }
"else"			{ count(); return(ELSE); }
"enum"			{ count(); 
	if (typedef_start_flag)
		typedef_start_flag = 2;
	return(ENUM); 
	//typedef_start_flag를 괄호로 감싸져 있다는 것을 나타내는 2로 설정합니다.
	}
"extern"		{ count(); return(EXTERN); }
"float"			{ count(); return(FLOAT); }
"for"			{ count(); return(FOR); }
"goto"			{ count(); return(GOTO); }
"if"			{ count(); return(IF); }
"int"			{ count(); return(INT); }
"long"			{ count(); return(LONG); }
"register"		{ count(); return(REGISTER); }
"return"		{ count(); return(RETURN); }
"short"			{ count(); return(SHORT); }
"signed"		{ count(); return(SIGNED); }
"sizeof"		{ count(); return(SIZEOF); }
"static"		{ count(); return(STATIC); }
"struct"		{ count(); 
	if (typedef_start_flag)
		typedef_start_flag = 2;
	return(STRUCT); 
	//typedef_start_flag를 괄호로 감싸져 있다는 것을 나타내는 2로 설정합니다.
	}
"switch"		{ count(); return(SWITCH); }
"union"			{ count(); 
	if (typedef_start_flag)
		typedef_start_flag = 2;
	return(UNION); 
	//typedef_start_flag를 괄호로 감싸져 있다는 것을 나타내는 2로 설정합니다.
	}
"unsigned"		{ count(); return(UNSIGNED); }
"void"			{ count(); return(VOID); }
"volatile"		{ count(); return(VOLATILE); }
"while"			{ count(); return(WHILE); }

{L}({L}|{D})*		{ count(); return(check_type()); }

0[xX]{H}+{IS}?		{ count(); return(CONSTANT); }
0{D}+{IS}?		{ count(); return(CONSTANT); }
{D}+{IS}?		{ count(); return(CONSTANT); }
L?'(\\.|[^\\'])+'	{ count(); return(CONSTANT); }

{D}+{E}{FS}?		{ count(); return(CONSTANT); }
{D}*"."{D}+({E})?{FS}?	{ count(); return(CONSTANT); }
{D}+"."{D}*({E})?{FS}?	{ count(); return(CONSTANT); }

L?\"(\\.|[^\\"])*\"	{ count(); return(STRING_LITERAL); }

"..."			{ count(); return(ELLIPSIS); }
">>="			{ count(); return(RIGHT_ASSIGN); }
"<<="			{ count(); return(LEFT_ASSIGN); }
"+="			{ count(); return(ADD_ASSIGN); }
"-="			{ count(); return(SUB_ASSIGN); }
"*="			{ count(); return(MUL_ASSIGN); }
"/="			{ count(); return(DIV_ASSIGN); }
"%="			{ count(); return(MOD_ASSIGN); }
"&="			{ count(); return(AND_ASSIGN); }
"^="			{ count(); return(XOR_ASSIGN); }
"|="			{ count(); return(OR_ASSIGN); }
">>"			{ count(); return(RIGHT_OP); }
"<<"			{ count(); return(LEFT_OP); }
"++"			{ count(); return(INC_OP); }
"--"			{ count(); return(DEC_OP); }
"->"			{ count(); return(PTR_OP); }
"&&"			{ count(); return(AND_OP); }
"||"			{ count(); return(OR_OP); }
"<="			{ count(); return(LE_OP); }
">="			{ count(); return(GE_OP); }
"=="			{ count(); return(EQ_OP); }
"!="			{ count(); return(NE_OP); }
";"			{ count(); return(';'); }
("{"|"<%")		{ count(); 
	if (typedef_start_flag == 2)
	{
		if (curly_brace_flag == -1)
			curly_brace_flag = 0;
		curly_brace_flag++;
	// 만약 괄호로 감싸져 있어야하는 자료형인 경우에는 자료형의 선언 시작과 끝을 구분하기 위해 curly_brace_flag를 이용하여 괄호의 짝을 맞춥니다.
	}
	return('{'); }
("}"|"%>")		{ count(); 
	if (typedef_start_flag == 2)
		curly_brace_flag--;
	// 만약 괄호로 감싸져 있어야하는 자료형인 경우에는 자료형의 선언 시작과 끝을 구분하기 위해 curly_brace_flag를 이용하여 괄호의 짝을 맞춥니다.
	return('}'); }
","			{ count(); return(','); }
":"			{ count(); return(':'); }
"="			{ count(); return('='); }
"("			{ count(); return('('); }
")"			{ count(); return(')'); }
("["|"<:")		{ count(); return('['); }
("]"|":>")		{ count(); return(']'); }
"."			{ count(); return('.'); }
"&"			{ count(); return('&'); }
"!"			{ count(); return('!'); }
"~"			{ count(); return('~'); }
"-"			{ count(); return('-'); }
"+"			{ count(); return('+'); }
"*"			{ count(); return('*'); }
"/"			{ count(); return('/'); }
"%"			{ count(); return('%'); }
"<"			{ count(); return('<'); }
">"			{ count(); return('>'); }
"^"			{ count(); return('^'); }
"|"			{ count(); return('|'); }
"?"			{ count(); return('?'); }

[ \t\v\n\f]		{ count(); }
.			{ /* ignore bad characters */ } 

%%

//입력 스트림의 끝(EOF)에 도달하면 입력을 종료시키는 함수
int yywrap()
{
	return(1);
}

/*
comment함수는 여러 줄 주석을 처리하기 위한 함수로 "*"문자가 나오기 전까지 모든 문자를 무시하고 
이후 다음 문자가 "/"이라면 종료하고 아니라면 다시 "*"문자 확인 과정으로 돌아가서 반복합니다.
*/
void comment()
{
	char c, c1;

	while (1)
	{
		c = input();
		if (c == '*')
		{
			if ((c1 = input()) != '/' && c != 0)
				unput(c1);
			else
				break;
		}
	}
}

void lineComment()
{
	char c, c1;

	while ((c = input()) != '\n' && c != 0)
		;
}

/*
아래는
ANSI C grammar, LEX에 있던 부분으로 현재 열 번호를 저장하여 에러메세지에 활용할 때 사용하였습니다. 
*/
int column = 0;
void count()
{
	int i;

	for (i = 0; yytext[i] != '\0'; i++)
		if (yytext[i] == '\n')
			column = 0;
		else if (yytext[i] == '\t')
			column += 8 - (column % 8);
		else
			column++;
}

/*
아래 함수는 변수의 자료형을 확인하는 함수로 각 case에 맞는 flag를 확인 및 수정 후 알맞은 자료형을 반환합니다.
*/
int check_type()
{
	// 만약 typedef가 앞에 붙어있는 자료형인 경우 세 개의 케이스로 나눠서 처리하였습니다.
	if (typedef_start_flag)
	{
		/*
		typedef_start_flag가 2라서 중괄호를 이용하려 선언하는 자료형이고,
		curly_brace_flag가 0으로 모든 중괄호의 쌍이 맞게 끝난 경우는
		선언이 완료되고 별칭이 붙는 경우이다.
		따라서 이 경우는 플래그를 초기화하고 자료형을 추가한다.
		*/
		if (typedef_start_flag == 2 && curly_brace_flag == 0)
		{
			typedef_start_flag = 0;
			add_typedef(yytext);
		}
		/*
		typedef_start_flag가 1인 경우는 중괄호가 필요하지 않는 자료형 혹은 변수이므로 바로 선언이 완료된 것으로
		판단하고 모든 플래그를 초기화하고 자료형을 추가한다.
		*/
		else if (typedef_start_flag == 1)
		{
			typedef_start_flag = 0;
			curly_brace_flag = 0;
			add_typedef(yytext);
		}
		/*
		마지막으로 typedef_start_flag가 2이므로 중괄호가 필요한 자료형인데 curly_brace_flag가 -1인 경우는
		아직 중괄호를 만나지 않고 해당 자료형의 이름만 만난 부분이므로, 현재 이름이 선언되었다는 표시로
		curly_brace_flag를 0으로 바꾸고 별칭을 만날 때까지 다시 어휘 분석을 진행한다.
		*/
		else if (typedef_start_flag == 2 && curly_brace_flag == -1)
			curly_brace_flag = 0;
	}
	/*
	아래 조건문은 현재 입력 단어가 typedef, define로 이전에 선언된 변수 혹은 자료형인지 확인 후 각각에 맞는 토큰을 반환하고
	만약 둘 다 아니라면 일반 식별자인 IDENTIFIER을 반환한다.
	*/
	if (is_typedef(yytext)) {
        return TYPE_NAME;   /* typedef 이름 */
    }
    else if (is_define(yytext)) {
        return CONSTANT;   /* #define 상수 이름 */
    }
    else {
        return IDENTIFIER; /* 일반 식별자 */
    }
}
