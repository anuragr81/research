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
%token BLANK
%token DOT
%token SEMICOLON
%token STRUCT
%token<sValue> NAME
%type<sValue> constant
%type<sValue> statement
%type<sValue> variable

%%

statement : variable EQ variable {$$=$1;} | variable EQ constant {$$=$1;}
variable : NAME { $$ = addVariable($1); } | variable DOT NAME { char buf[100]; $$ = structElement(buf,$1,$3);}
| variable SQR_OPEN INTEGER SQR_CLOSE { char buf[100]; arrayElementConstIndex(buf,$1,atoi($3)); $$=buf; }
| variable SQR_OPEN variable SQR_CLOSE { char buf[100]; arrayElementVarIndex(buf,$1,$3); $$=buf; }
constant : INTEGER { char buf[100]; $$ = createInteger(buf,$1); } | DOUBLE { char buf[100]; $$ = createDouble(buf,$1); }
%%
/*
* statement : variable predicate
structure : STRUCT BRACKET_OPEN NAME COMMA NAME BRACKET_CLOSE { $$ = $3 ; }
statement : NAME BRACKET_OPEN operand BRACKET_CLOSE { $$=$1;}

operand : operand COMMA | single_operand

single_operand : function | variable { $$=checkVariable(); } // expression
*/
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
printVariables();

free(m.input);
cleanup();
return 0;
}
#endif
