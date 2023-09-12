#include "listset/listset.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char const *argv[]) {
    printf("%c", 127);
    return 0;
}

struct hashtable {
    int size;
    struct listset **table;
};

struct hashtable *hashtable_new(int size) {
    struct hashtable *result = malloc(sizeof(struct hashtable));
    result->size = size;

    result->table = malloc(sizeof(struct listset *) * size);
    for (size_t i = 0; i < size; i++) {
        result->table[i] = listset_new();
    }
    return result;
};

int *find_freqs(char *text, int length) {
    int *freqs = calloc(256, sizeof(int));
    for (size_t i = 0; i < length; i++) {
        unsigned char index = text[i];
        freqs[index]++;
    }
    return freqs;
}
