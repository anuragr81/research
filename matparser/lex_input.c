%{
#include "y.tab.h"
void yyerror(char *);
%}

%%

[ \t]*\[[ \t]* { return SQR_OPEN ; }
[ \t]*\][ \t]* { return SQR_CLOSE ;}
[ \t]*\([ \t]* { return BRACKET_OPEN; }
[ \t]*\)[ \t]* { return BRACKET_CLOSE; }

struct { return STRUCT ; }
[A-Za-z]+ { yylval.sValue=(char*)malloc(sizeof(char)*strlen(yytext)); strcpy(yylval.sValue,yytext); return NAME; }

\= { return EQ ; }
[0-9]+ { return INTEGER ; }
[0-9]+\.[0-9]+ { return DOUBLE ; }

[\,] { return COMMA; }
[ \t] { return BLANK; } /* skip whitespace */
[\.] { return DOT; }

[ \t]*[\;][ \t]* { return SEMICOLON; }

. yyerror("Unknown character");

%%

int yywrap(void) {
return 1 ;
}


