#include "bitset.h"

const int bits_in_word = sizeof(0U) * 8; // 8 bits in a byte, 4 bytes in unsigned int

// create a new, empty bit vector set with a universe of 'size' items
struct bitset *bitset_new(int size) {
    struct bitset *set = calloc((size / sizeof(0U) + 1), sizeof(0U));
    set->universe_size = size;
    set->size_in_words = (size / bits_in_word) + 1;
    set->bits = malloc(sizeof(0U) * set->size_in_words);
    set->bits = calloc(set->size_in_words, sizeof(0U) * set->size_in_words);
    return set;
}

// get the size of the universe of items that could be stored in the set
int bitset_size(struct bitset *this) { return this->universe_size; }

// get the number of items that are stored in the set
int bitset_cardinality(struct bitset *this) {
    int total = 0;
    for (int i = 0; i < this->size_in_words; i++) {
        unsigned int word = this->bits[i];
        // takes in unsigned (int) and returns hamming weight as int
        total = total + __builtin_popcount(word);
    }
    printf("%d\n", total);
    return total;
}

// check to see if an item is in the set
int bitset_lookup(struct bitset *this, int item) {
    int i = item / bits_in_word;
    int pos = item % bits_in_word;
    unsigned mask = 1 << pos;
    unsigned bit = this->bits[i] & mask;
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
    unsigned int mask = 1 << pos;
    this->bits[i] = this->bits[i] | mask;
}

// remove an item with number 'item' from the set
void bitset_remove(struct bitset *this, int item) {
    int i = item / bits_in_word;
    int pos = item % bits_in_word;
    this->bits[i] = this->bits[i] & ~(1 << pos);
}

// place the union of src1 and src2 into dest;
// all of src1, src2, and dest must have the same size universe
void bitset_union(struct bitset *dest, struct bitset *src1, struct bitset *src2) {
    int i = dest->universe_size, j = src1->universe_size, k = src2->universe_size;
    if (i != j && j != k) {
        return;
    }

    for (int i = 0; i < dest->size_in_words; i++) {
        dest->bits[i] = src1->bits[i] | src2->bits[i];
    }
}

// place the intersection of src1 and src2 into dest
// all of src1, src2, and dest must have the same size universe
void bitset_intersect(struct bitset *dest, struct bitset *src1, struct bitset *src2) {
    int i = dest->universe_size, j = src1->universe_size, k = src2->universe_size;
    if (i != j && j != k) {
        return;
    }

    for (int i = 0; i < dest->size_in_words; i++) {
        dest->bits[i] = src1->bits[i] & src2->bits[i];
    }
}