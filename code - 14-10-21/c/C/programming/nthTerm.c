#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int find_nth_term(int n, int a, int b, int c) {
    // Return (n-1) + (n-2) + (n-3). If n == 1, 2, or 3, return a, b, or c respectively.
    switch (n) {
    case 1:
        return a;
    case 2:
        return b;
    case 3:
        return c;

    default:
        return find_nth_term(n - 1, a, b, c) + find_nth_term(n - 2, a, b, c) + find_nth_term(n - 3, a, b, c);
    }
}

int main() {
    int n, a, b, c;

    scanf("%d %d %d %d", &n, &a, &b, &c);
    int ans = find_nth_term(n, a, b, c);

    printf("%d", ans);
    return 0;
}