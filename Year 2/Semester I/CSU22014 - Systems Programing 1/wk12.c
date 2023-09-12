#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

struct huffchar {
    int freq;
    int is_compound;

    union {
        struct {
            struct huffchar* left;
            struct huffchar* right;
        } compound;
        unsigned char c;
    } u;
};

void print_huffman_codes(struct huffchar* root, int* path, int depth) {
    if (root->is_compound == 0) {
        printf("%c ", root->u.c);
        for (int i = 0; i < depth; i++){
            printf("%d ", path[i]);
        }
        printf("\n");

    } else {
        print_huffman_codes(root->u.compound.left, path, depth+1);
        path[depth] = 0;
        print_huffman_codes(root->u.compound.right, path, depth+1);
        path[depth] = 1;
    }
}
