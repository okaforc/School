%{
# include "romcalc.tab.h"
void yyerror(char *s);
%}
%option nodefault

%%
Z { yylval = 0; return NUM; }
I { yylval = 1; return NUM; }
IV { yylval = 4; return NUM; }
V { yylval = 5; return NUM; }
IX { yylval = 9; return NUM; }
X { yylval = 10; return NUM; }
XL { yylval = 40; return NUM; }
L { yylval = 50; return NUM; }
XC { yylval = 90; return NUM; }
C { yylval = 100; return NUM; }
CD { yylval = 400; return NUM; }
D { yylval = 500; return NUM; }
CM { yylval = 900; return NUM; }
M { yylval = 1000; return NUM; }

ZZ {yyerror("syntax error");}
VV { yyerror("syntax error"); }
LL { yyerror("syntax error"); }
DD { yyerror("syntax error"); }
IIII { yyerror("syntax error"); }
XXXX { yyerror("syntax error"); }
CCCC { yyerror("syntax error"); }


"+" { return ADD; }
"-" { return SUB; }
"*" { return MUL; }
"/" { return DIV; }
"{" { return OP; }
"}" { return CP; }

\n { return EOL; }
[ \t] { }
. { yyerror("syntax error"); }

%%
