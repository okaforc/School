#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

struct huffnode {
    int freq;
    int is_compound;

    union {
        struct {
            struct huffnode* left;
            struct huffnode* right;
        } compound;
        char c;
    } u;
};

struct huffnode* huffnode_newleaf(unsigned char c, int freq) {
    struct huffnode* leaf;
    leaf = malloc(sizeof(struct huffnode));
    leaf->freq = freq;
    leaf->is_compound = 0;
    leaf->u.c = c;
    return leaf;
}

struct huffnode* find_least_frequent(struct huffnode** list, int nchars, int* vacant) {
    int min_ind = 0, min_freq = INT_MAX;
    for (int i = 1; i < nchars; i++) {
        if (list[i] != NULL) {
            if (list[i]->freq < min_freq) {
                min_freq = list[i]->freq;
                min_ind = i;
            }
        }
    }
    *vacant = min_ind;
    struct huffnode* result = list[min_ind];
    list[min_ind] = NULL;
    return result;
}

// struct huffnode* find_least_frequent(struct huffnode** list, int nchars) {
//     int min_ind = 0, min_freq = INT_MAX;
//     for (int i = 1; i < nchars; i++) {
//         if (list[i] != NULL) {
//             if (list[i]->freq < min_freq) {
//                 min_freq = list[i]->freq;
//                 min_ind = i;
//             }
//         }
//     }
//     struct huffnode* result = list[min_ind];
//     list[min_ind] = NULL;
//     return result;
// }

struct huffnode* huffnode_newcompound(struct huffnode* left, struct huffnode* right) {
    struct huffnode* fork;
    fork = malloc(sizeof(struct huffnode*));
    fork->u.compound.left = left;
    fork->u.compound.right = right;
    fork->is_compound = 1;
    fork->freq = left->freq + right->freq;
    return fork;
}

struct huffnode* build_huffman_tree(int* freqs, int nchars) {
    struct huffnode** list;
    list = malloc(sizeof(struct huffman*) * nchars);
    for (int i = 0; i < nchars; i++) {
        list[i] = huffnode_newleaf(i, freqs[i]);
    }
    int vacant;
    struct huffnode* left = find_least_frequent(list, nchars, &vacant); // min is then set to null
    struct huffnode* right = find_least_frequent(list, nchars, &vacant);

    list[vacant] = huffnode_newcompound(left, right);
}

