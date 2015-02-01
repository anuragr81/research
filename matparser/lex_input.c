%{
#include "y.tab.h"
void yyerror(char *);
%}

%%

[ \t]*\[[ \t]* { return SQR_OPEN ; }
[ \t]*\][ \t]* { return SQR_CLOSE ;}
[ \t]*\([ \t]* { return BRACKET_OPEN; }
[ \t]*\)[ \t]* { return BRACKET_CLOSE; }

[ \t]*struct[ \t]* { return STRUCT ; }
[A-Za-z]+ { yylval.sValue=(char*)malloc(sizeof(char)*strlen(yytext)); strcpy(yylval.sValue,yytext); return NAME; }

[ \t]*\=[ \t]* { return EQ ; }
[ \t]*[0-9]+[ \t]* { return INTEGER ; }
[ \t]*[0-9]+\.[0-9]+[ \t]* { return DOUBLE ; }

[ \t]*[\,][ \t]* { return COMMA; }
[ \t] { return BLANK; } /* skip whitespace */
[\.] { return DOT; }

[ \t]*[\;][ \t]* { return SEMICOLON; }

[ \t]*[\:][ \t]* { return COLON; }

. yyerror("Unknown character");

%%

int yywrap(void) {
return 1 ;
}


