%{
char errstring[] = "syntax error\n";
int flag = 0;
int open = 0;
int ignore = 0;
%}
%option nodefault

%%
(\*\*)(.)* { ; } 
\"(.)*\" { if (!ignore) printf("%s", yytext); }
\} { open--; ignore = 0;  if (open < 0) {flag = 1; return 0;} }
\{ { if (!ignore) {open++;} ignore = 1; }
. {if (!ignore) printf("%s", yytext); }
\n {if (!ignore) printf("%s", yytext); }
%%

int main() {
    yylex();
    if (flag || open != 0) {
        printf("%s", errstring);
    }
    return 0;
}
