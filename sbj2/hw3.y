%{

#include <stdio.h>

int ary[9] = {0,0,0,0,0,0,0,0,0};

//배열의 인덱스를 헷갈리지 않게하기위해 가독성을 높이는 전처리를 실시하였습니다.
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
int idx; // 어떤 자료형의 count를 증가시켜야할지 나타내는 인덱스 변수입니다.
int pointerFlag; // 이 변수가 포인터 변수인지 자료형 변수인지 확인하는 플래그입니다.
int arrayFlag; // 배열을 대괄호를 기준으로 셀 때 다차원 배열을 중복으로 count하지 않기 위한 flag입니다.
void yyerror(const char *msg); // 디버깅 용으로 사용하였습니다.
%}

%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELLIPSIS

%token CASE DEFAULT IF ELSEIF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
%start translation_unit

%%
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
	: function_definition
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


/*
	예전 c문법은 변수의 선언은 무조건 가장 앞에서만 가능했고 테스트코드처럼 선언문과 할당문이 섞일 수 없었습니다. 
	따라서 declaration_list 후 statement_list가 나올 수 있었던 기존의 코드를 수정하여 두 심볼이 섞여서
	나올 수 있는 compound_list를 새로 만들었습니다.
*/
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
// 소괄호가 들어간 부분은 함수 호출 또는 선언 부분이므로 하수 호출 카운트를 증가하였습니다.
// 또한 포인터 오퍼레이터, 증가, 감소 연산이 있는 부분은 연산자 호출을 증가하였습니다.
postfix_expression
	: primary_expression
	| postfix_expression '[' expression ']'
	| postfix_expression '(' ')' {ary[FUNCTION]++;} 
	| postfix_expression '(' argument_expression_list ')' {ary[FUNCTION]++;}
	| postfix_expression '.' IDENTIFIER {ary[OPERATOR]++;} // 구조체 접근 연산자
	| postfix_expression PTR_OP IDENTIFIER  {ary[OPERATOR]++;} // 포인터 연산자
	| postfix_expression INC_OP {ary[OPERATOR]++;} // 증가연산자
	| postfix_expression DEC_OP {ary[OPERATOR]++;} // 감소연산자
	;


unary_expression
	: postfix_expression
	| INC_OP unary_expression {ary[OPERATOR]++;} // 증가 연산자
	| DEC_OP unary_expression {ary[OPERATOR]++;} // 감소 연산자
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
	| '(' type_name ')' cast_expression {ary[OPERATOR]++;} // 캐스팅은 연사자로 취급하여 카운트하였습니다.
	;

multiplicative_expression
	: cast_expression
	| multiplicative_expression '*' cast_expression {ary[OPERATOR]++;} // 곱하기 연산자 카운트
	| multiplicative_expression '/' cast_expression {ary[OPERATOR]++;} // 나누기 연산자 카운트
	| multiplicative_expression '%' cast_expression {ary[OPERATOR]++;} // 나머지 연산자 카운트
	;

additive_expression
	: multiplicative_expression
	| additive_expression '+' multiplicative_expression {ary[OPERATOR]++;} // 더하기 연산자 카운트
	| additive_expression '-' multiplicative_expression {ary[OPERATOR]++;} // 빼기 연산자 카운트
	;

shift_expression
	: additive_expression
	| shift_expression LEFT_OP additive_expression {ary[OPERATOR]++;} // 쉬프트 연산자 카운트
	| shift_expression RIGHT_OP additive_expression {ary[OPERATOR]++;} // 쉬프트 연산자 카운트
	;

relational_expression
	: shift_expression
	| relational_expression '<' shift_expression {ary[OPERATOR]++;} // 비교연산자 카운트
	| relational_expression '>' shift_expression {ary[OPERATOR]++;} // 비교연산자 카운트
	| relational_expression LE_OP shift_expression {ary[OPERATOR]++;} // 비교연산자 카운트
	| relational_expression GE_OP shift_expression {ary[OPERATOR]++;} // 비교연산자 카운트
	;

equality_expression
	: relational_expression
	| equality_expression EQ_OP relational_expression {ary[OPERATOR]++;} // 비교연산자 카운트
	| equality_expression NE_OP relational_expression {ary[OPERATOR]++;} // 비교연산자 카운트
	;

and_expression
	: equality_expression
	| and_expression '&' equality_expression {ary[OPERATOR]++;} // 비트연산자 카운트
	;

exclusive_or_expression
	: and_expression 
	| exclusive_or_expression '^' and_expression {ary[OPERATOR]++;} // 비트연산자 카운트
	;

inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression '|' exclusive_or_expression {ary[OPERATOR]++;} // 비트연산자 카운트
	;

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression AND_OP inclusive_or_expression {ary[OPERATOR]++;} // 논리연산자 카운트
	;

logical_or_expression
	: logical_and_expression
	| logical_or_expression OR_OP logical_and_expression {ary[OPERATOR]++;} // 논리연산자 카운트
	;

conditional_expression
	: logical_or_expression
	| logical_or_expression '?' expression ':' conditional_expression // 삼항연산자는 카운트하지 않았습니다.
	;

/* operation */
assignment_operator
	: '=' {ary[OPERATOR]++;} // 대입연산자 증가
	| MUL_ASSIGN {ary[OPERATOR]++;} // 산술대입연산자 증가
	| DIV_ASSIGN {ary[OPERATOR]++;} // 산술대입연산자 증가
	| MOD_ASSIGN {ary[OPERATOR]++;} // 산술대입연산자 증가
	| ADD_ASSIGN {ary[OPERATOR]++;} // 산술대입연산자 증가
	| SUB_ASSIGN {ary[OPERATOR]++;} // 산술대입연산자 증가
	| LEFT_ASSIGN {ary[OPERATOR]++;} // 시프트대입연산자 증가
	| RIGHT_ASSIGN {ary[OPERATOR]++; } // 시프트대입연산자 증가
	| AND_ASSIGN {ary[OPERATOR]++;} // 비트대입연산자 증가
	| XOR_ASSIGN {ary[OPERATOR]++;} // 비트대입연산자 증가
	| OR_ASSIGN {ary[OPERATOR]++;} // 비트대입연산자 증가
	;
/*
타입의 종류별로 인덱스를 구분하는데 출력에 필요로하는 int와 char의 인덱스만 저장하고 나머지는 인덱스를 -1로 만들어서 
카운트 할 type이 아니라는 것을 저장해둠.
*/

type_specifier
	: VOID {idx = -1;}
	| CHAR {idx = CHARACTER;}
	| SHORT {idx = -1;}
	| LONG {idx = -1;}
	| DOUBLE {idx = -1;}
	| SIGNED {idx = -1;}
	| INT {idx = INTEGER;}
	| UNSIGNED {idx = -1;}
	| FLOAT {idx = -1;}
	| TYPE_NAME {idx = -1;}
	| struct_or_union_specifier {idx = -1;}
	| enum_specifier {idx = -1;}
	;

/* iteration count */
//while과 do while, for문은 LOOP로 카운트한다.
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
	: pointer direct_declarator {
		ary[POINTER]++;
		if (arrayFlag)
		{
			ary[ARRAY]++;
			arrayFlag = 0;
			idx = -1;
		}
		}
		/*
		포인터의 수를 여기서 증가시킵니다. 포인터는 여러번 반복되어도 하나의 포인터 카운트만 증가되어야 하므로 '*'의 한 번 이상의 반복으로 이루어진
		pointer 심볼이 나오는 경우만 카운트를 증가시키면 중복 증가가 발생하지 않습니다.
		또한 대괄호 기준으로 배열 수를 카운트하면 다차원 배열은 여러개의 배열로 중복 카운트 될 수 있기 때문에 대괄호가 나오면 arrayFlag를 1로 바꾸고
		그러한 배열이 포함되어 있을 수 있는 direct_declarator 심볼이 반환되었을 때 플래그를 확인하고 배열 수를 카운트하였습니다.
		또한 포인터의 배열인 경우 저장한 자료형은 int나 char이 아닌 8바이트 포인터이기 때문에 더블포인터와 마찬가지로 자료형의 선언으로 보지 않았습니다.
		그래서 idx를 -1로 설정하여 어떤 자료형의 카운트도 증가시키지 않았습니다. 
		*/
	| direct_declarator {
		if (arrayFlag)
			ary[ARRAY]++;
		arrayFlag = 0;
	}// 이 부분도 위의 array 카운트 논리와 동일합니다.
	;


pointer
	: '*'
	| '*' type_qualifier_list
	| '*' pointer {idx = -1;}// 자료형 증가 x
	| '*' type_qualifier_list pointer {idx = -1;}// 자료형 증가 x
	;
/*
	여기서 포인터가 이중 포인터 이상이 되는 순간 int, char의 개수는 증가시키지 않고 포인터의 개수만 증가되어야 하기 때문에
	포인터가 반복되면 증가시킬 자료형을 나타내는 idx의 값을 -1로 변경하여 어떠한 자료형의 카운트도 증가시키지 않는것을
	나타내었습니다.
*/

//대괄호가 존재하면 배열의 선언임을 알 수 있고, 이를 상위 심볼에서 활용하기 위해 arrayFlag를 1로 변경하였습니다.
direct_declarator
	: IDENTIFIER
	| '(' declarator ')'
	| direct_declarator '[' constant_expression ']' {arrayFlag = 1;} 
	| direct_declarator '[' ']' {arrayFlag = 1;}
	| direct_declarator '(' parameter_type_list ')' {ary[FUNCTION]++; idx = -1;} 
	| direct_declarator '(' identifier_list ')' {ary[FUNCTION]++; idx = -1;}
	| direct_declarator '(' ')' {ary[FUNCTION]++; idx = -1;}
	;
	/*
	위 세 코드는 함수의 선언 부분으로 이 경우 리턴값을 나타내는 부분에서 int나 char가 올 수 있는데 해당 자료형으로 변수를 선언한 것이 아니라 함수의 리턴값을 명시한 부분입니다.
	따라서 자료형은 증가하지 않고, 함수의 카운트는 증가해야하기 때문에 함수의 카운트를 증가시키고 idx를 -1로 두어 자료형 카운트를 증가하지 않는것을 나타내였습니다.
	*/

//if문과 switch문만 Selection문으로 카운트 하였습니다.
//또한 기존의 if else문의 구성이 아닌, if else if else의 구성으로 변경하였습니다.
selection_statement
	: IF '(' expression ')' statement {ary[SELECTION]++;}
	| ELSEIF '(' expression ')' statement
	| ELSE statement
	| SWITCH '(' expression ')' statement {ary[SELECTION]++;}
	;

//RETURN 토큰이 나오면 return의 카운트를 증가하였습니다.
jump_statement
	: GOTO IDENTIFIER ';'
	| CONTINUE ';'
	| BREAK ';'
	| RETURN ';' {ary[RET]++;}
	| RETURN expression ';' {ary[RET]++;}

declaration
	: declaration_specifiers ';' 
	| declaration_specifiers init_declarator_list ';' 
	;

/* 
변수의 선언이 이루어지는 심볼로 단순히 INT, CHAR 토큰이 들어왔을 때 증가시키면 더블 포인터, 또는 int a, b;이런 코드에서 정확한 타입 개수를 세지 못합니다.
따라서 선언이 이루어지는 단위로 나눠진 심볼인 init_declarator가 반복될 때마다 선언 자료형이 int, char이라면 카운트를 증가시키는 방식으로 구현하였습니다.
*/
init_declarator_list 
	: init_declarator {
		if (idx != -1)
			ary[idx]++;
	}
	| init_declarator_list ',' init_declarator   {
		if (idx != -1)
			ary[idx]++;
		idx = -1;
	}
	;

//대입 연산자의 카운트를 증가하였습니다.
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

/*	
	여기서 init_declarator와 마찬가지로 매개변수의 선언이 이루어지는 부분에서도 변수의 타입별 증가가 이루어질 수 있도록
	동일한 로직으로 구현하였습니다.
*/
parameter_declaration
	: declaration_specifiers declarator
	{
		if (idx != -1)
			ary[idx]++;
		idx = - 1;
	}
	| declaration_specifiers abstract_declarator {idx = -1;}
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

//아래와같은 배열과 함수타입의 중첩 코드는 나오지 않는다고 말씀해주셔서 별도 처리는 하지 않았습니다.
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

/*
	아래 코드도 init_declarator와 마찬가지로 구조체 내부 변수 선언이 이루어지는 부분에서도 변수의 타입별 증가가 이루어질 수 있도록
	동일한 로직으로 구현하였습니다.
*/
struct_declarator_list
	: struct_declarator{
		if (idx != -1)
			ary[idx]++;
	}
	| struct_declarator_list ',' struct_declarator{
		if (idx != -1)
			ary[idx]++;
		idx = -1;
	}
	;

struct_declarator
	: declarator
	| ':' constant_expression
	| declarator ':' constant_expression
	;
		
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
    fprintf(stderr,"%s\n", msg);
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

