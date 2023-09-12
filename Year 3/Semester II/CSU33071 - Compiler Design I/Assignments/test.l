%{
int evens = 0;
%}
%option nodefault

%%
[a-z]. {printf("valid\n");}
\n {;}
. {;}
%%

int main() {
    yylex();
    /* printf("%8d\n", evens); */
    return 0;
}
