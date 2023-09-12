#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// include the header files with the declarations of listset
#include "listset.h"
#include "hashtable.h"

// create a new, empty linked list set
struct listset *listset_new() {
    struct listset *header;
    header = malloc(sizeof(struct listset));
    header->head = NULL;
    return header;
}

/* check to see if an item is in the set
   returns 1 if in the set, 0 if not */
int listset_lookup(struct listset *list, char *item) {
    for (struct listnode *p = list->head; p != NULL; p = p->next) {
        if (strcmp(item, p->str) == 0) {
            return 1;
        }
    }
    return 0;
}

// add an item, with number 'item' to the set
// has no effect if the item is already in the set.
// New items that are not already in the set should
// be added to the start of the list
void listset_add(struct listset *list, char *item) {
    struct listnode *newnode = malloc(sizeof(struct listnode));
    newnode->str = item;
    newnode->next = list->head;
    list->head = newnode;
}

// remove an item with number 'item' from the set
void listset_remove(struct listset *list, char *item) {
    if (!listset_lookup(list, item) || list->head == NULL) {
        return;
    }

    struct listnode *a = malloc(sizeof(struct listnode));
    a->next = list->head;
    int i = 1;
    for (struct listnode *b = list->head; b != NULL; a = a->next, b = b->next) {
        if (strcmp(b->str, item) == 0) {
            a->next = b->next;
            if (i == 1) {
                // at head
                list->head = b->next;
            }
            free(b);
            return;
        }
        i = 0;
    }
}

// place the union of src1 and src2 into dest
void listset_union(struct listset *dest, struct listset *src1, struct listset *src2) {
    for (struct listnode *ap = src1->head; ap != NULL; ap = ap->next) {
        listset_add(dest, ap->str);
    }

    for (struct listnode *ap = src2->head; ap != NULL; ap = ap->next) {
        listset_add(dest, ap->str);
    }
}

// place the intersection of src1 and src2 into dest
void listset_intersect(struct listset *dest, struct listset *src1, struct listset *src2) {
    for (struct listnode *ap = src1->head; ap != NULL; ap = ap->next) {
        // for (struct listnode *bp = src2->head; bp != NULL; bp = bp->next) {
        // }
        if (listset_lookup(src2, ap->str)) {
            listset_add(dest, ap->str);
        }
    }
}

// return the number of items in the listset
int listset_cardinality(struct listset *list) {
    int size = 0;
    struct listnode *p = list->head;
    while (p != NULL) {
        size++;
        p = p->next;
    }
    return size;
}

// print the elements of the list set
void listset_print(struct listset *list) {
    struct listnode *p;

    for (p = list->head; p != NULL; p = p->next) {
        printf("%s, ", p->str);
    }
    printf("\n");
}


// compute a hash of a string using a seed value, where the result
// falls between zero and range-1
int hash_string(char* string, int range) {
    int i;
    int hash = 0;
    const int HASH_SEED = 19;

    // simple loop for mixing the input string
    for (i = 0; string[i] != '\0'; i++) {
        hash = hash * HASH_SEED + string[i];
    }
    // check for unlikely case that hash is negative
    if (hash < 0) {
        hash = -hash;
    }
    // bring the hash within the range 0..range-1
    hash = hash % range;

    // printf("str: \'%s\', hash: %d\n", string, hash);
    return hash;
}

// create a new empty hashtable
struct hashtable* hashtable_new(int size) {
    struct hashtable* ht = malloc(sizeof(struct hashtable));
    ht->size = size;
    ht->table = malloc(sizeof(struct listset*) * size);
    for (size_t i = 0; i < size; i++) {
        ht->table[i] = listset_new();
    }
    return ht;
}

// add an item to the hashtable
void hashtable_add(struct hashtable* htable, char* item) {
    int hash = hash_string(item, htable->size);
    struct listset* list = htable->table[hash];
    listset_add(list, item);
}

// return 1 if item is in hashtable, 0 otherwise
int hashtable_lookup(struct hashtable* htable, char* item) {
    for (size_t i = 0; i < htable->size; i++) {
        if (listset_lookup(htable->table[i], item)) {
            return 1;
        }
    }
    return 0;
}








// print the elements of the hashtable set
void hashtable_print(struct hashtable* htable) {
    for (int i = 0; i < htable->size; i++) {
        listset_print(htable->table[i]);
    }
}

int main(int argc, char const *argv[]) {
    struct listset *list = listset_new();
    struct hashtable *table = hashtable_new(255);
    listset_add(list, "a");
    listset_add(list, "b");
    listset_add(list, "c");
    listset_add(list, "d");
    listset_add(list, "e");
    listset_add(list, "f");

    // struct listset *list2 = listset_new();
    // listset_add(list2, "1");
    // listset_add(list2, "2");
    // listset_add(list2, "3");
    // listset_add(list2, "4");
    // listset_add(list2, "5");

    // struct listset *list3 = listset_new();
    // listset_union(list3, list, list2);
    // listset_print(list3);
    // list3 = listset_new();
    // listset_intersect(list3, list, list2);
    // listset_print(list3);

    printf("%d\n", listset_cardinality(list));
    listset_print(list);
    listset_remove(list, "a");
    listset_print(list);
    listset_remove(list, "b");
    listset_print(list);
    listset_remove(list, "f");
    listset_print(list);
    listset_remove(list, "e");
    listset_print(list);
    printf("%d\n", listset_cardinality(list));
    listset_add(list, "world");
    listset_print(list);
    printf("%d\n", listset_cardinality(list));

    hashtable_add(table, "hello");
    hashtable_print(table);
    printf("%d\n", hashtable_lookup(table, "hello"));
    hashtable_add(table, "hello");
    printf("%d\n", hashtable_lookup(table, "hello"));
    
    return 0;
}
