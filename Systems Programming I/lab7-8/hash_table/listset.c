#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// include the header files with the declarations of listset
#include "listset.h"

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
    if (!listset_lookup(list, item)) {
        struct listnode *newnode = malloc(sizeof(struct listnode));
        newnode->str = item;
        newnode->next = list->head;
        list->head = newnode;
    }
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
        // loop nodes a, and b, where a is before head, b is head (looping)
        if (strcmp(b->str, item) == 0) {
            a -> next = b -> next;
            if (i == 1) {
                // at head
                list->head = b -> next;
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
