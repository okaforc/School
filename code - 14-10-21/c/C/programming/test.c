#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    char s[100];

    scanf("%[^\n]%*c", &*s);
    printf("Hello, World!\n");
    printf(s);

    return 0;
}