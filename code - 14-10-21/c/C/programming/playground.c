#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    printf("Size of char ......... = %2d byte(s)\n", sizeof(char));
    printf("Size of short ........ = %2d byte(s)\n", sizeof(short));
    printf("Size of int .......... = %2d byte(s)\n", sizeof(int));
    printf("Size of long long .... = %2d byte(s)\n", sizeof(long long));
    printf("Size of long ......... = %2d byte(s)\n", sizeof(long));
    printf("Size of unsigned char. = %2d byte(s)\n", sizeof(unsigned char));
    printf("Size of unsigned int.. = %2d byte(s)\n", sizeof(unsigned int));
    printf("Size of unsigned short = %2d byte(s)\n", sizeof(unsigned short));
    printf("Size of unsigned long  = %2d byte(s)\n", sizeof(unsigned long));
    printf("Size of float ........ = %2d byte(s)\n", sizeof(float));
    printf("Size of double ....... = %2d byte(s)\n", sizeof(double));
    printf("Size of long double .. = %2d byte(s)\n", sizeof(long double));
    return 0;
}