#include <stdio.h>

void update(int *a, int *b) {
    int *ca = a;
    *a += *b;
    *b = (*a - *b < 0 ? (*a - *b) * -1 : *a - *b);
    printf("\n%d\n\n", *b);
}

int main() {
    int a, b;
    int *pa = &a, *pb = &b;

    scanf("%d %d", &a, &b);
    update(pa, pb);
    printf("%d\n%d", a, b);

    return 0;
}