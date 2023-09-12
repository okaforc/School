#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>


unsigned char getBits(unsigned char c, int start, int dist) {
    unsigned char p = c >> start;
    unsigned char q = (1 << dist) - 1;
    return q & p;
}

int main(int argc, char const *argv[]) {
    unsigned char c = 0b10100111;
    unsigned char t = getBits(c, 0, 2);
    printf("%d", t);
    return 0;
}