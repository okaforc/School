%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
char *itor(int v);
int nsum = 0; // numeral sum, for calculating a roman numeral only
int res = 0; // expression sum
%}
%token NUM OP CP
%token ADD SUB
%token MUL DIV
%token EOL
%%

calclist:
|   calclist numeral EOL { 
    // printf("%d = %s\n", $2, itor($2));
    printf("%s\n", itor($2));
}
|   calclist exp EOL { 
    printf("%s\n", itor(res));
    // printf("%d = %s\n", res, itor(res));
    res = 0; 
}
;

exp: factor 
|   exp ADD factor { 
        int tres = $1 + $3; 
        // printf("ADD: %d + %d = %d\n", $1, $3, tres); 
        res = tres; 
        $$ = tres; 
    }
|   exp SUB factor { 
        int tres = $1 - $3; 
        // printf("SUB: %d - %d = %d\n", $1, $3, tres); 
        res = tres; 
        $$ = tres; 
    }
;

factor: numeral
|   factor MUL numeral { 
        int tres = $1 * $3; 
        // printf("MUL: %d x %d = %d\n", $1, $3, tres); 
        res = tres; 
        $$ = tres; 
    }
|   factor DIV numeral { 
        if ($3 == 0) {
            yyerror("syntax error");
        } 
        int tres = $1 / $3; 
        // printf("DIV: %d / %d = %d\n", $1, $3, tres); 
        res = tres; 
        $$ = tres; 
    }
;

numeral:
|   OP exp CP {$$ = $2;}
|   numeral val {
        // printf("%d\n", $2); 
        nsum = 0; 
        $$ = $2;
    }
;

val: NUM
|   val NUM {
    if (($1 == 900 || $1 == 400 || $1 == 90 || $1 == 40 || $1 == 9 || $1 == 4) && 
        !($2 == 900 || $2 == 400 || $2 == 90 || $2 == 40 || $2 == 9 || $2 == 4) &&
        ($2 > ($1%9) || $2 > ($1%4))) {
        yyerror("syntax error");
    } else if ($2 > $1) {
        yyerror("syntax error");
    }
    $$ = nsum + $1 + $2;
}
;
%%
// convert an integer to roman numerals
char *itor (int v) {
    static char s[1024] = "";
    int neg;
    s[0] = '\0'; // "reset" char array
    if (v < 0) {
        strcat(s, "-");
    }
    else if (v > 0) {
        // if positive, proceed as normal
    } else {
        return "Z";
    }
    int tv = abs(v);
    int vals[13] = {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1}; // valid roman numeral counterparts
    char *rnums[14] = {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I", "\0"}; // roman numerals
    int N = 13;
    for (int i = 0; i < N; i++) {
        while (tv >= vals[i]) {
            tv -= vals[i];
            strcat(s, rnums[i]);
        }
    }
    strcat(s, rnums[13]);
    return s;
}

int yyerror(char *s) {
    fprintf(stderr, "%s\n", s);
    exit(0);
}

int main() {
    yyparse();
    return 0;
}