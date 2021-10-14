#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {

    char ch;
    char s[100];
    char sen[100];

    scanf("%c", &ch);
    scanf("%[^\n]%*c", &s);
    scanf("\n");
    scanf("%c", &sen);

    printf("%c", ch);
    printf("%c", s);
    printf("%c", sen);

    return 0;
}