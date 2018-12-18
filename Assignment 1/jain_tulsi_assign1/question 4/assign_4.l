%{
#include <iostream>
#include <stdlib.h>
#include "assign_4.tab.h"  /* We want the tokens defined in assign_4.y. */
%}

%option noyywrap

%%


[0-9]+                  { return NUM; }
"function"				{ return FUNCTION; }
"typeof"				{ return TYPEOF; }
"if"					{ return IF; }
"var"					{ return VAR;  }
"for"					{ return FOR;  }
"return"				{ return RETURN;  }
"true"					{ return TRUE; }
"false"					{ return FALSE;}
"null"					{ return NULLTOKEN; }
"("                     { return LPAREN; }
")"                     { return RPAREN; }
"{"                     { return LCURLY; }
"}"                     { return RCURLY; }
"["                     { return LSQUARE; }
"]"                     { return RSQUARE; }
"+"                     { return PLUS; }
"-"                     { return MINUS; }
"while" 				{ return WHILE;}
","						{ return COMMA;}
"."						{ return DOT;}
"="						{ return EQUAL;}
"&&"					{ return AND;}
"|"						{ return PIPE;}
"||"					{ return DOUBLEPIPE;}
";"						{ return SEMICOLON;}
":"						{ return COLON;}
"\""					{ return DOUBLEQUOTES;}
"?"						{ return QUESTIONMARK;}
"!="					{return NOT_EQUAL;}
"//".* 				{return COMMENTS;}
[a-zA-Z]+\.[_a-zA-Z0-9]+   { return APPLYFUNCTION;}
[a-zA-Z]+\.[_a-zA-Z0-9]+\.[_a-zA-Z0-9]+ { return APPLYFUNCTION2;}
[a-zA-Z][_a-zA-Z0-9]*   { return IDENTIFIER;}
[ \t\r\n]+              { /* Ignore white characters. */ }
.                       { std::cerr << "Invalid token" << std::endl; exit(1); }

%%
