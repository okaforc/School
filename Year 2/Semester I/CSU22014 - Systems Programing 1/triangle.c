#include <stdio.h>
#include <stdlib.h>
#include <math.h>

struct triangle
{
    int a;
    int b;
    int c;
};

typedef struct triangle triangle;
void sort_by_area(triangle* tr, int n) {
    int* ts = malloc(sizeof(int) * n); // size of respective triangles in triangles (^)

    for (int i = 0; i < n; i++) {
        triangle t = tr[i];
        float p = (t.a + t.b + t.c) / 2.0;
        float pa = p - t.a;
        float pb = p - t.b;
        float pc = p - t.c;

        float square = (p * pa * pb * pc);
        ts[i] = square;
    }
    
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n - i - 1; j++) {
            if (ts[j] > ts[j + 1]) {
                int temp = ts[j];
                ts[j] = ts[j + 1];
                ts[j + 1] = temp;

                // bubble sort ts and tr by swapping the side lengths

                temp = tr[j].a;
                tr[j].a = tr[j + 1].a;
                tr[j + 1].a = temp;

                temp = tr[j].b;
                tr[j].b = tr[j + 1].b;
                tr[j + 1].b = temp;

                temp = tr[j].c;
                tr[j].c = tr[j + 1].c;
                tr[j + 1].c = temp;
            }
        }
    }
}

// 3
// 7 24 25
// 5 12 13
// 3 4 5

int main()
{
    int n;
    scanf("%d", &n);
    triangle* tr = malloc(n * sizeof(triangle));
    for (int i = 0; i < n; i++) {
        scanf("%d%d%d", &tr[i].a, &tr[i].b, &tr[i].c);
    }
    sort_by_area(tr, n);
    for (int i = 0; i < n; i++) {
        printf("%d %d %d\n", tr[i].a, tr[i].b, tr[i].c);
    }
    return 0;
}