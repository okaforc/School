%{
#include <stdio.h>
#include <stdlib.h>
int sum = 0;
%}
%token NUM
%token EOL
%%

numeral:
|   numeral val EOL {printf("%d\n", $2); sum = 0;}
;

val: NUM
|   val NUM {
    if ($2 > 3999 || $2 < 1 || $$ > 3999 || sum > 3999) {yyerror("syntax error");}
    else {
        if (($1 == 900 || $1 == 400 || $1 == 90 || $1 == 40 || $1 == 9 || $1 == 4) && 
            !($2 == 900 || $2 == 400 || $2 == 90 || $2 == 40 || $2 == 9 || $2 == 4) &&
            ($2 > ($1%9) || $2 > ($1%4))) {
            yyerror("syntax error");
        } else if ($2 > $1) {
            yyerror("syntax error");
        }
        $$ = sum + $1 + $2;
    }
}
;
%%
int yyerror(char *s) {
    printf("%s\n", s);
    exit(0);
}

int main() {
    yyparse();
    return 0;
}