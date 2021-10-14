#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {

    int n, sum = 0, pow = 10, p = 0;
    scanf("%d", &n);
    for (int i = 0; i < 5; i++) {
        p += (n % pow) - 2 * sum;
        p /= (pow / 10);
        sum += p;
        printf("%d\n", sum);
        pow *= 10;
    }
    return 0;

    printf("%d", sum);
}