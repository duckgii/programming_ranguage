%{

#include <stdio.h>

int ary[9] = {0,0,0,0,0,0,0,0,0};

#define	FUNCTION	0
#define OPERATOR	1
#define INTEGER		2
#define CHARACTER	3
#define POINTER		4
#define ARRAY		5
#define SELECTION	6
#define LOOP		7
#define RET			8

int yylex(void);
struct YYLTYPE;
int idx; // int a, b; 이런 경우 해결을 위한 count 변수     
void yyerror(const char *msg); 
%}

%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELLIPSIS

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
%start translation_unit

%locations
%%
/* for function count */
/* function declaration */
translation_unit
	: external_declaration
	| translation_unit external_declaration
	;


function_definition
	: declaration_specifiers declarator compound_statement
	| declarator compound_statement
	;

external_declaration
	: function_definition {ary[FUNCTION]++;}
	| declaration
	;

declaration_specifiers
	: storage_class_specifier
	| storage_class_specifier declaration_specifiers
	| type_specifier
	| type_specifier declaration_specifiers
	| type_qualifier
	| type_qualifier declaration_specifiers
	;
//선언과 정의가 섞여있는부분 에러 발생

compound_statement
	: '{' compound_list '}'
	;

compound_list
	: declaration
	| statement
	| compound_list declaration
	| compound_list statement
	;

/* function call */
postfix_expression
	: primary_expression
	| postfix_expression '[' expression ']'
	| postfix_expression '(' ')' {ary[FUNCTION]++;}
	| postfix_expression '(' argument_expression_list ')' {ary[FUNCTION]++;}
	| postfix_expression '.' IDENTIFIER {ary[OPERATOR]++;}
	| postfix_expression PTR_OP IDENTIFIER  {ary[OPERATOR]++;}
	| postfix_expression INC_OP {ary[OPERATOR]++;}
	| postfix_expression DEC_OP {ary[OPERATOR]++;}
	;


/* for counting Operation */
unary_expression
	: postfix_expression
	| INC_OP unary_expression {ary[OPERATOR]++;}
	| DEC_OP unary_expression {ary[OPERATOR]++;}
	| unary_operator cast_expression
	| SIZEOF unary_expression
	| SIZEOF '(' type_name ')'

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression
	;

/* operation */
unary_operator
	: '&' 
	| '*' 
	| '+' 
	| '-' 
	| '~' 
	| '!' 
	;

cast_expression
	: unary_expression
	| '(' type_name ')' cast_expression {ary[OPERATOR]++;}
	;

multiplicative_expression
	: cast_expression
	| multiplicative_expression '*' cast_expression {ary[OPERATOR]++;}
	| multiplicative_expression '/' cast_expression {ary[OPERATOR]++;}
	| multiplicative_expression '%' cast_expression {ary[OPERATOR]++;}
	;

additive_expression
	: multiplicative_expression
	| additive_expression '+' multiplicative_expression {ary[OPERATOR]++;}
	| additive_expression '-' multiplicative_expression {ary[OPERATOR]++;}
	;

shift_expression
	: additive_expression
	| shift_expression LEFT_OP additive_expression {ary[OPERATOR]++;}
	| shift_expression RIGHT_OP additive_expression {ary[OPERATOR]++;}
	;

relational_expression
	: shift_expression
	| relational_expression '<' shift_expression {ary[OPERATOR]++;}
	| relational_expression '>' shift_expression {ary[OPERATOR]++;}
	| relational_expression LE_OP shift_expression {ary[OPERATOR]++;}
	| relational_expression GE_OP shift_expression {ary[OPERATOR]++;}
	;

equality_expression
	: relational_expression
	| equality_expression EQ_OP relational_expression {ary[OPERATOR]++;}
	| equality_expression NE_OP relational_expression {ary[OPERATOR]++;}
	;

and_expression
	: equality_expression
	| and_expression '&' equality_expression {ary[OPERATOR]++;}
	;

exclusive_or_expression
	: and_expression
	| exclusive_or_expression '^' and_expression {ary[OPERATOR]++;}
	;

inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression '|' exclusive_or_expression {ary[OPERATOR]++;}
	;

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression AND_OP inclusive_or_expression {ary[OPERATOR]++;}
	;

logical_or_expression
	: logical_and_expression
	| logical_or_expression OR_OP logical_and_expression {ary[OPERATOR]++;}
	;

conditional_expression
	: logical_or_expression
	| logical_or_expression '?' expression ':' conditional_expression
	;

/* operation */
assignment_operator
	: '=' {ary[OPERATOR]++;}
	| MUL_ASSIGN {ary[OPERATOR]++;}
	| DIV_ASSIGN {ary[OPERATOR]++;}
	| MOD_ASSIGN {ary[OPERATOR]++;}
	| ADD_ASSIGN {ary[OPERATOR]++;}
	| SUB_ASSIGN {ary[OPERATOR]++;}
	| LEFT_ASSIGN {ary[OPERATOR]++;}
	| RIGHT_ASSIGN {ary[OPERATOR]++;}
	| AND_ASSIGN {ary[OPERATOR]++;}
	| XOR_ASSIGN {ary[OPERATOR]++;}
	| OR_ASSIGN {ary[OPERATOR]++;}
	;

type_specifier
	: VOID
	| CHAR {idx = CHARACTER;}
	| SHORT
	| INT {idx = INTEGER;}
	| LONG 
	| FLOAT
	| DOUBLE
	| SIGNED
	| UNSIGNED
	| TYPE_NAME
	| struct_or_union_specifier
	| enum_specifier
	;

/* iteration count */
iteration_statement
	: WHILE '(' expression ')' statement {ary[LOOP]++;}
	| DO statement WHILE '(' expression ')' ';' {ary[LOOP]++;}
	| FOR '(' expression_statement expression_statement ')' statement {ary[LOOP]++;}
	| FOR '(' expression_statement expression_statement expression ')' statement {ary[LOOP]++;}
	;

statement
	: compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	;

/* for counting pointer */

declarator
	: pointer direct_declarator {ary[POINTER]++;}
	| direct_declarator
	;

pointer
	: '*'
	| '*' type_qualifier_list
	| '*' pointer
	| '*' type_qualifier_list pointer
	;

/* for counting array*/

direct_declarator
	: IDENTIFIER
	| '(' declarator ')'
	| direct_declarator '[' constant_expression ']' {ary[ARRAY]++;}
	| direct_declarator '[' ']' {ary[ARRAY]++;}
	| direct_declarator '(' parameter_type_list ')' {ary[FUNCTION]++;}
	| direct_declarator '(' identifier_list ')'
	| direct_declarator '(' ')'
	;

/* for counting selection */
selection_statement
	: IF '(' expression ')' statement {ary[SELECTION]++;}
	/*| IF '(' expression ')' statement ELSE statement {ary[SELECTION]++;}*/
	| ELSE statement
	| SWITCH '(' expression ')' statement {ary[SELECTION]++;}
	;

/* for counting return */
jump_statement
	: GOTO IDENTIFIER ';'
	| CONTINUE ';'
	| BREAK ';'
	| RETURN ';' {ary[RET]++;}
	| RETURN expression ';' {ary[RET]++;}

/* 추가 필요 문법 */
declaration
	: declaration_specifiers ';'
	| declaration_specifiers init_declarator_list ';'
	;

init_declarator_list
	: init_declarator {ary[idx]++;};
	| init_declarator_list ',' init_declarator {ary[idx]++;}
	;

init_declarator
	: declarator
	| declarator '=' initializer {ary[OPERATOR]++;}
	;

initializer
	: assignment_expression
	| '{' initializer_list '}'
	| '{' initializer_list ',' '}'
	;

initializer_list
	: initializer
	| initializer_list ',' initializer
	;

type_qualifier
	: CONST
	| VOLATILE
	;

primary_expression
	: IDENTIFIER
	| CONSTANT
	| STRING_LITERAL
	| '(' expression ')'
	;

expression
	: assignment_expression
	| expression ',' assignment_expression
	;


argument_expression_list
	: assignment_expression
	| argument_expression_list ',' assignment_expression
	;

type_name
	: specifier_qualifier_list
	| specifier_qualifier_list abstract_declarator
	;

expression_statement
	: ';'
	| expression ';'
	;


type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;

constant_expression
	: conditional_expression
	;


parameter_type_list
	: parameter_list
	| parameter_list ',' ELLIPSIS
	;

parameter_list
	: parameter_declaration
	| parameter_list ',' parameter_declaration
	;

parameter_declaration
	: declaration_specifiers declarator
	| declaration_specifiers abstract_declarator
	| declaration_specifiers
	;

identifier_list
	: IDENTIFIER
	| identifier_list ',' IDENTIFIER
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list
	| type_specifier
	| type_qualifier specifier_qualifier_list
	| type_qualifier
	;

abstract_declarator
	: pointer
	| direct_abstract_declarator
	| pointer direct_abstract_declarator
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
	| '[' ']'
	| '[' constant_expression ']'
	| direct_abstract_declarator '[' ']'
	| direct_abstract_declarator '[' constant_expression ']'
	| '(' ')'
	| '(' parameter_type_list ')'
	| direct_abstract_declarator '(' ')'
	| direct_abstract_declarator '(' parameter_type_list ')'
	;

// for typedef
storage_class_specifier
	: TYPEDEF
	| EXTERN
	| STATIC
	| AUTO
	| REGISTER
	;

struct_or_union
	: STRUCT
	| UNION
	;

struct_or_union_specifier
	: struct_or_union IDENTIFIER '{' struct_declaration_list '}' 
	| struct_or_union '{' struct_declaration_list '}'
	| struct_or_union IDENTIFIER 
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: specifier_qualifier_list struct_declarator_list ';'
	;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;

struct_declarator
	: declarator
	| ':' constant_expression
	| declarator ':' constant_expression
	;
		
//for enum
enum_specifier
	: ENUM '{' enumerator_list '}'
	| ENUM IDENTIFIER '{' enumerator_list '}'
	| ENUM IDENTIFIER
	;

enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

enumerator
	: IDENTIFIER
	| IDENTIFIER '=' constant_expression
	;

%%

void yyerror(const char *msg)
{
    /* 전역 YYLTYPE yylloc 에 마지막 토큰 위치가 들어 있음 */
    fprintf(stderr,
            "Error at %d:%d: %s\n",
            yylloc.first_line,
            yylloc.first_column,
            msg);
}

int main(void)
{
	yyparse();
	printf("function = %d\n", ary[0]);
	printf("operator = %d\n", ary[1]);
	printf("int = %d\n", ary[2]);
	printf("char = %d\n", ary[3]);
	printf("pointer = %d\n", ary[4]);
	printf("array = %d\n", ary[5]);
	printf("selection = %d\n", ary[6]);
	printf("loop = %d\n", ary[7]);
	printf("return = %d\n", ary[8]);
	return 0;
}

