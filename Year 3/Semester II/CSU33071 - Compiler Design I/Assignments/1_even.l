%{
int evens = 0;
%}
%option nodefault

%%
[0-9]*[02468]+[ \n]+ { ; }
\n { ; }
. { ; }
%%

int main() {
    yylex();
    printf("%8d\n", evens);
    return 0;
}
