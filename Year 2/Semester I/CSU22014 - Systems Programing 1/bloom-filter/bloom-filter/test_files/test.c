// include the standard library header files that we use in this
// program, which are denoted by angle brackets < >
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// include the header files that are not part of the standard library,
// but can be found in the current directory, which are denoted with
// quotation marks " "
#include "../bitset.h"
#include "../bloom.h"

const int bits_in_word = sizeof(0ULL) * 8; // 8 bits in a byte, 4 bytes in unsigned int

/* Open a file. Abort progam if file cannot be opened */
FILE *open_file(char filename[]) {
    FILE *file;

    file = fopen(filename, "r");
    if (file == NULL) {
        printf("FATAL: Error opening file %s. Aborting program.\n", filename);
        exit(1);
    }

    return file;
}

/* read lines of text from a file; return number of lines */
int read_strings(char *filename, char **strings, int size) {
    const int max_line = 1024;
    char line[max_line];
    int i = 0;
    FILE *file;
    char *eof;

    file = open_file(filename);

    /* read in the strings - one per line */
    eof = fgets(line, max_line, file);
    while (eof != NULL && i < size) { /* eof == NULL => end of file */
        int length = strlen(line);
        strings[i] = malloc(sizeof(char) * (length + 1));
        strcpy(strings[i], line);
        i++;
        eof = fgets(line, max_line, file);
    }

    fclose(file);
    return i;
}



void add_chars_to_set(struct bitset *this, char *s) {
    int i;
    for (i = 0; s[i] != 0; i++) {
        unsigned char temp = s[i];
        bitset_add(this, temp);
        // printf("%c\n", temp);
    }
}
// print the contents of the bitset
void bitset_print(struct bitset *this) {
    int i;
    int size = bitset_size(this);
    for (i = 0; i < size; i++) {
        if (bitset_lookup(this, i) == 1) {
            printf("%d ", i);
        }
    }
    printf("\n");
}

// print the contents of the bloom filter
void bloom_print(struct bloom *this) { bitset_print(this->bitset); }



// create a new, empty bit vector set with a universe of 'size' items
struct bitset *bitset_new(int size) {
    struct bitset *set = calloc((size / sizeof(0ULL) + 1), sizeof(struct bitset));
    set->universe_size = size;
    set->size_in_words = (size / bits_in_word) + 1;
    set->bits = calloc(set->size_in_words, sizeof(0ULL) * set->size_in_words);
    return set;
}

// get the size of the universe of items that could be stored in the set
int bitset_size(struct bitset *this) { return this->universe_size; }

// get the number of items that are stored in the set
int bitset_cardinality(struct bitset *this) {
    int total = 0;
    for (int i = 0; i <= this->size_in_words; i++) {
        uint64_t word = this->bits[i];
        // takes in unsigned (int) and returns hamming weight as int
        total = total + __builtin_popcountll(word);
    }
    printf("%d\n", total);
    return total;
}

// check to see if an item is in the set
int bitset_lookup(struct bitset *this, int item) {
    int i = item / bits_in_word;
    int pos = item % bits_in_word;
    uint64_t mask = 1ULL << pos;
    uint64_t bit = this->bits[i] & mask;
    bit = bit >> (item % bits_in_word);
    if (bit == 1) {
        return 1;
    }
    return 0;
}

// add an item, with number 'item' to the set
// has no effect if the item is already in the set
void bitset_add(struct bitset *this, int item) {
    int i = item / bits_in_word;
    int pos = item % bits_in_word;
    uint64_t mask = 1ULL << pos;
    this->bits[i] = this->bits[i] | mask;
}

// remove an item with number 'item' from the set
void bitset_remove(struct bitset *this, int item) {
    int i = item / bits_in_word;
    int pos = item % bits_in_word;
    this->bits[i] = this->bits[i] & ~(1ULL << pos);
}

// place the union of src1 and src2 into dest;
// all of src1, src2, and dest must have the same size universe
void bitset_union(struct bitset *dest, struct bitset *src1, struct bitset *src2) {
    int i = dest->universe_size, j = src1->universe_size, k = src2->universe_size;
    if (i == j && j == k) {
        for (int i = 0; i < dest->size_in_words; i++) {
            dest->bits[i] = src1->bits[i] | src2->bits[i];
        }
    }
}

// place the intersection of src1 and src2 into dest
// all of src1, src2, and dest must have the same size universe
void bitset_intersect(struct bitset *dest, struct bitset *src1, struct bitset *src2) {
    int i = dest->universe_size, j = src1->universe_size, k = src2->universe_size;
    if (i == j && j == k) {
        for (int i = 0; i < dest->size_in_words; i++) {
            dest->bits[i] = src1->bits[i] & src2->bits[i];
        }
    }
}

const int BLOOM_HASH1 = 17;
const int BLOOM_HASH2 = 29;

// compute a hash of a string using a seed value, where the result
// falls between zero and range-1
int hash_string(char *string, int seed, int range) {
    int i;
    int hash = 0;

    // simple loop for mixing the input string
    for (i = 0; string[i] != '\0'; i++) {
        hash = hash * seed + string[i];
    }
    // check for unlikely case that hash is negative
    if (hash < 0) {
        hash = -hash;
    }
    // bring the hash within the range 0..range-1
    hash = hash % range;

    return hash;
}

// create a new, empty Bloom filter of 'size' items
struct bloom *bloom_new(int size) {
    struct bloom *bl = calloc((size / sizeof(0U) + 1), sizeof(0U));
    struct bitset *set = bitset_new(size);
    bl->bitset = set;
    bl->size = size;
    return bl;
}

// check to see if a string is in the set
int bloom_lookup(struct bloom *this, char *item) {
    int seeds[] = {BLOOM_HASH1, BLOOM_HASH2};
    int nhashes = 2; // number of hash seeds
    for (int i = 0; i < nhashes; i++) {
        int hash = hash_string(item, seeds[i], this->size);
        printf("hash - %d\n", hash);
        if (bitset_lookup(this->bitset, hash) == 0) {
            return 0;
        }
    }
    return 1;
}

// add a string to the set
// has no effect if the item is already in the set
void bloom_add(struct bloom *this, char *item) {
    int hash = hash_string(item, BLOOM_HASH1, this->size);
    printf("hash 1: %d\n", hash);
    bitset_add(this->bitset, hash);
    hash = hash_string(item, BLOOM_HASH2, this->size);
    printf("hash 2: %d\n", hash);
    bitset_add(this->bitset, hash);
    bitset_print(this->bitset);
}

// place the union of src1 and src2 into dest
void bloom_union(struct bloom *dest, struct bloom *src1, struct bloom *src2) { 
    struct bitset *set1 = src1->bitset;
    struct bitset *set2 = src2->bitset;
    bitset_union(dest->bitset, set1, set2);
}

// place the intersection of src1 and src2 into dest
void bloom_intersect(struct bloom *dest, struct bloom *src1, struct bloom *src2) {
    struct bitset *set1 = src1->bitset;
    struct bitset *set2 = src2->bitset;
    bitset_intersect(dest->bitset, set1, set2);
}

void test_bloom_filter(char **strings, int nstrings) {
    struct bloom *filter = bloom_new(256);
    int i;

    for (i = 0; i < nstrings; i++) {
        // fprintf(stderr, "%d: %s\n", i, strings[i]);
        bloom_add(filter, strings[i]);
    }
    bloom_print(filter);
}

void test_bitset_intersect(char *string1, char *string2) {
    struct bitset *a = bitset_new(256);
    struct bitset *b = bitset_new(256);
    struct bitset *c = bitset_new(256);

    add_chars_to_set(a, string1);
    add_chars_to_set(b, string2);

    // compute and print the intersection of sets
    bitset_intersect(c, a, b);
    bitset_print(c);
}

int main(int argc, char const *argv[]) {
    struct bitset *a = bitset_new(256);
    struct bitset *b = bitset_new(256);
    struct bitset *c = bitset_new(256);
    struct bitset *d = bitset_new(256);
    struct bitset *e = bitset_new(256);

    struct bloom *x = bloom_new(256);
    struct bloom *y = bloom_new(256);
    struct bloom *z = bloom_new(256);
    // bloom_add(x, "hello");
    // bloom_add(x, "world");
    // printf("%d\n", bloom_lookup(x, "world"));
    printf("%d\n", sizeof(0ULL));
    char *strings[128];
    int nstrings = read_strings("test_inputs\\test_bitset_intersect_1.txt", strings, 128);
    test_bitset_intersect(strings[0], strings[1]);
    // bitset_add(a, 52);
    // bitset_add(a, 22);
    // bitset_print(a);

    // bloom_add(x, "hello world");
    // bloom_add(y, "hello world");
    // bloom_print(x);
    // bloom_print(y);
    // bloom_union(z, x, y);
    // bloom_print(z);

    // bitset_add(a, 0);
    // bitset_add(a, 1);
    // bitset_add(a, 2);
    // bitset_add(a, 3);
    // bitset_add(a, 4);
    // bitset_add(a, 5);
    // bitset_add(a, 6);
    // bitset_add(a, 7);
    // bitset_add(a, 8);
    // bitset_add(a, 9);
    // bitset_add(a, 10);
    // bitset_add(a, 11);

    // char *t = "h";
    // add_chars_to_set(a, t);
    // bitset_add(a, 104);
    // bitset_print(a);

    // for (int i = 0; i < a->size_in_words; i++) {
    //     printf("%u\n", a->bits[i]);
    // }

    // printf("isin: %d\n", bitset_lookup(a, 4));
    // printf("isin: %d\n", 'h');
    // bitset_add(c, 32);

    // bitset_add(a, 123);
    // bitset_remove(a, 14);
    // printf("look for %d: %d\n", 123, bitset_lookup(a, 123));
    // // print the cardinality of the bitset
    // printf("cardinality is %d\n", bitset_cardinality(a));
    return 0;
}
