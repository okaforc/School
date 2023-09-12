%{
char errstring[] = "INVALID";
int cyr = 23; // current year (2023)
int flag = 0;
char* years[1024];
int yearcnt = 0;

// 132-D-123
// 141-RN-44444
// 12-LM-1


//         161-C-567


// 88-G-890


// 4444-
// 00-z-9999
// 02-D-9999999
// 88-T-1234

%}
%option nodefault
NUMBER  [0-9]{1,6}
NUMBER_INV  [0-9]{7,}
COUNTY_CURR  C|CE|CN|CW|D|DL|G|KE|KK|KY|L|LD|LH|LM|LS|MH|MN|MO|OY|RN|SO|T|W|WH|WX|WW
COUNTY_PAST  C|CE|CN|CW|D|DL|G|KE|KK|KY|L|LK|LD|LH|LM|LS|MH|MN|MO|OY|RN|SO|TN|TS|W|WD|WH|WX|WW
YEAR_OLD  [0-9]{2}
YEAR_NEW  [0-9]{3}

%%
{YEAR_OLD}-{COUNTY_PAST}-{NUMBER_INV} {years[yearcnt] = errstring; yearcnt++;}
{YEAR_OLD}-{COUNTY_PAST}-{NUMBER} {
    char tmpyr[] = {yytext[0], yytext[1], '\0'}; 
    int diff = atoi(tmpyr); 
    if (diff >= 87 && diff <= 99) {
        diff -= 100;
    }

    if (diff < 87 && diff > 12) {
        years[yearcnt] = errstring;
        yearcnt++;
    } else {
        char *tmp = malloc(sizeof(char)*3);
        sprintf(tmp, "%d", abs(diff-cyr));
        years[yearcnt] = tmp;
        yearcnt++;
    }
}
{YEAR_NEW}-{COUNTY_CURR}-{NUMBER} {
    char tmpyr[] = {yytext[0], yytext[1], '\0'}; 
    int diff = atoi(tmpyr); 
    if (diff < 13 || (yytext[2] != '1' && yytext[2] != '2')) {
        years[yearcnt] = errstring;
        yearcnt++;
    } else {
        char *tmp = malloc(sizeof(char)*3);
        sprintf(tmp, "%d", abs(diff-cyr));
        years[yearcnt] = tmp;
        yearcnt++;
    }
}

.-.-. { years[yearcnt] = errstring; yearcnt++; }
.-.- { years[yearcnt] = errstring; yearcnt++; }
.- { years[yearcnt] = errstring; yearcnt++; }
\n { ; }
. { ; }

%%

int main() {
    yylex();
    for (int i = 0; i < yearcnt; i++) {
        printf("%s\n", years[i]);
    }
    return 0;
}
