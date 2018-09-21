%option noyywrap
%{
	#include "cfg.tab.h"
	int f(char*);
	int lc=0;
%}

whitespace [ \t]
linefeed \n
%%
"++" {return INC;}
"--" {return DCR;}
"+"	{return PLUS;}
"-" {return MINUS;}
"*" {return STAR;}
"/"	{return DIV;}
"%" {return MOD;}
"!=" {return UNEQUAL;}
"<<" {return LEFTSHIFT;}
">>" {return RIGHTSHIFT;}
"<=" {return LTE;}
">=" {return GTE;}
"==" {return EQUAL;}
"!" {return NOT;}
"<" {return LT;}
">" {return GT;}
"="	{return ASSIGN;}
"||" {return OR;}
"&&" {return AND;}
"|" {return BITWISE_OR;}
"&" {return BITWISE_AND;}
"~" {return BITWISE_NOT;}

"(" {return LP;}
")" {return RP;}
"[" {return SLP;}
"]" {return SRP;}
"{" {return FLP;}
"}" {return FRP;}

"," {return COMMA;}
";" {return SEMI;}
"." {return DOT; }
":" {return COLON;}
"\\" {return BACKSLASH;}
"\'" {return SQ; }
"\"" {return DQ; }

("int") {yylval.type_val=0;return DATATYPE;}
("float") {yylval.type_val=1;return DATATYPE;}
("double") {yylval.type_val=2;return DATATYPE;}
("char") {yylval.type_val=3;return DATATYPE;}
("long") {yylval.type_val=4;return DATATYPE;}
"for" {yylval.type_val=lc++;return FOR;}
"while" {yylval.type_val=lc++;return WHILE;}
"if" {yylval.type_val=lc++;return IF;}
"ife" {yylval.type_val=lc++;return IFE;}
"else" {return ELSE;}


[ \t]+ { }

\"[^\"\n]*\" {return STRING_CONSTANT;}
\'[a-zA-Z]\' {strcpy(yylval.str,yytext);return CHAR_CONSTANT;}
[a-zA-Z][a-zA-Z0-9_]* {strcpy(yylval.str,yytext);return ID;}
[0-9]*[.][0-9]+ {strcpy(yylval.str,yytext);return FNUM;}
[0-9]+ {strcpy(yylval.str,yytext);return NUM;}

{linefeed}  {++yylineno;}
.	{return ERROR;}

%%

