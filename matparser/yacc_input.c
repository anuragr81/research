%{
#include <stdio.h>

#include <stdlib.h>
#include <string.h>
#include "cstructures.h"
    
//void processAssignment(char * subject, struct TVerb * tverb, char* object){
void processAssignment(char * subject, char* object){
printf("Subject=\"%s\"",subject);
printf("Object=\"%s\"",object);
}

struct Message {
char * input, * output;
};
int yylex(void);
void yyerror(char *);

static int error ;
%}

%union
{
int iValue;
char * sValue;
double dValue;
// struct Entity * pEntity;
// struct TVerb * pVerb;
};

%token <sValue> INTEGER
%token <sValue> DOUBLE

%token EQ
%token SQR_OPEN
%token SQR_CLOSE
%token BRACKET_OPEN
%token BRACKET_CLOSE
%token COMMA
%token<sValue> COLON
%token BLANK
%token DOT
%token SEMICOLON
%token STRUCT
%token<sValue> NAME

%type<sValue> constant

%type<sValue> matrix_decl
%type<sValue> matrix
%type<sValue> scalar
%type<sValue> row

%type<sValue> function
%type<sValue> arguments_list

%type<sValue> variable

%%

// TODO: document mandatory comma separator between columns in the matrix
// TODO: array indexing (including :)
// TODO: character strings
// TODO: for, if, while
// TODO: seq-vectors as 1:num
// TODO: matrix operators as - (+-/*^') and (.*/.^.') etc.

statement_decl : statement | statement SEMICOLON

statement : variable EQ variable {$1=$3;} 
                            | variable EQ constant {$1=$3;}
                            | variable EQ function {$1=$3;}
                                          | function
                                          
function  : NAME BRACKET_OPEN arguments_list BRACKET_CLOSE {$$=callFunctionOrMatrix($1,$3);}
                                          
arguments_list : arguments_list COMMA variable { $$=addToArgumentsList($1,$3); } 
                    | variable { $$=createArgumentsList(); }
                    | arguments_list COMMA constant { $$=addToArgumentsList($1,$3); }
                    | constant { $$=createArgumentsList(); }
                    | arguments_list COMMA COLON { $$=addToArgumentsList($1,$3); }
                    | COLON { $$=createArgumentsList(); }
                    | arguments_list COMMA function {  $$=addToArgumentsList($1,$3); }
                    | function { $$=createArgumentsList(); }

variable : NAME { $$ = addVariable($1); } 
                 | variable DOT NAME { char buf[100]; $$ = structElement(buf,$1,$3);}
                 
constant : scalar | matrix_decl

matrix_decl : SQR_OPEN matrix SQR_CLOSE { $$=$2;}

matrix: matrix SEMICOLON row {$$=addToMatrix($1,$3);} | row { $$=createMatrix();}

row : scalar { $$=createRow(); }
                          | row COMMA scalar { $$=addToRow($1,$3); }  
                          | row BLANK scalar { $$=addToRow($1,$3); }
                          
scalar : INTEGER { char buf[100]; $$ = createInteger(buf,$1); } | DOUBLE { char buf[100]; $$ = createDouble(buf,$1); }

%%

void yyerror(char *s)
{
printf( "%s\n", s);
error = 1;
}

#ifdef __BUILD_LIB__
void parseString(struct Message * m){
yy_scan_string(m->input);
printf("\n");
printf("\n",m->input);
yyparse();
printf("\n");
strcpy(m->output,output);

}
#else
int main(int argc,char * argv[])
{
initalizeExpression();
if (argc < 2)
{
printf("Usage: %s ",argv[1]);
return 1;
}
else
{
//printf("\n");
//printf("\n",argv[1]);
//printf("\n");
}
struct Message m;
size_t szInput = strlen(argv[1]);
m.input = (char*)malloc(szInput*sizeof(char));
strncpy(m.input,argv[1],szInput);

yy_scan_string(argv[1]);
yyparse();

if (error){
return 1;
}
//printVariables();

free(m.input);
cleanup();
return 0;
}
#endif
