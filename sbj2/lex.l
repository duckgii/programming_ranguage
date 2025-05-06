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
int typedef_start_flag;
int curly_brace_flag;
/* 심볼 테이블 최대 개수 */
#define MAX_SYMBOLS 1024

/* typedef 이름 테이블 */
static char *typedef_names[MAX_SYMBOLS];
static int   typedef_count = 0;
/* #define 이름 테이블 */
static char *define_names[MAX_SYMBOLS];
static int   define_count = 0;
/* struct 태그 이름 테이블 */
static char *struct_names[MAX_SYMBOLS];
static int   struct_count = 0;

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

void add_struct_tag(const char *s) {
    if (struct_count < MAX_SYMBOLS)
        struct_names[struct_count++] = strdup(s);
}

int is_struct_tag(const char *s) {
    for (int i = 0; i < struct_count; i++)
        if (strcmp(s, struct_names[i]) == 0)
            return 1;
    return 0;
}

void start_typedef(void)
{
	typedef_start_flag = 1; 
	curly_brace_flag = -1; 
}
%}


%%
"/*"			{ comment(); }
"//"			{ lineComment(); }
^"#include".*\n    { ; }
^"#define"[ \t]+{ID}.*\n {
	char namebuf[128];
    sscanf(yytext, "#%*s %127s", namebuf);
    add_define(namebuf);
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
"else"			{ count(); return(ELSE); }
"enum"			{ count(); 
	if (typedef_start_flag)
		typedef_start_flag = 2;
	return(ENUM); }
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
	return(STRUCT); }
"switch"		{ count(); return(SWITCH); }
"union"			{ count(); 
	if (typedef_start_flag)
		typedef_start_flag = 2;
	return(UNION); }
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
	}
	return('{'); }
("}"|"%>")		{ count(); 
	if (typedef_start_flag == 2)
		curly_brace_flag--;
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

int yywrap()
{
	return(1);
}


void comment()
{
	char c, c1;

loop:
	while ((c = input()) != '*' && c != 0)
		putchar(c);

	if ((c1 = input()) != '/' && c != 0)
	{
		unput(c1);
		goto loop;
	}

	if (c != 0)
		putchar(c1);
}

void lineComment()
{
	char c, c1;

loop:
	while ((c = input()) != '\n' && c != 0)
		putchar(c);

	if (c != 0)
		putchar(c1);
}


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

	ECHO;
}


int check_type()
{
	if (typedef_start_flag)
	{
		if (typedef_start_flag == 2 && curly_brace_flag == 0)
		{
			typedef_start_flag = 0;
			add_typedef(yytext);
		}
		else if (typedef_start_flag == 1)
		{
			typedef_start_flag = 0;
			curly_brace_flag = 0;
			add_typedef(yytext);
		}
		else if (typedef_start_flag == 2 && curly_brace_flag == -1)
			curly_brace_flag = 0;
	}
	if (is_typedef(yytext)) {
        return TYPE_NAME;   /* typedef 이름 */
    }
    else if (is_define(yytext)) {
        return CONSTANT;   /* #define 상수 이름 */
    }
    else if (is_struct_tag(yytext)) {
        return TYPE_NAME;   /* #struct 상수 이름 */
    }
    else {
        return IDENTIFIER; /* 일반 식별자 */
    }
}