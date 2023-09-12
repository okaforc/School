#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int K = 10;

struct list_node {
    float* items; // node can store up to K items
    int num_items; // number of items actually in node
    struct list_node* next;
};

struct sorted_list {
    struct list_node* head; // pointer to the first node of the list
};

struct sorted_list* new_list() {
    struct sorted_list* slist = malloc(sizeof(struct sorted_list*));
    slist->head = NULL;
    return slist;
}

struct list_node* new_node() {
    struct list_node* nnode = malloc(sizeof(struct list_node*));
    nnode->items = calloc(K, sizeof(float));
    nnode->next = NULL;
    nnode->num_items = 0;
    return nnode;
}

// set K, the maximum amount of values allowed in list_node
void set_max(int max) {
    K = max;
}

// look for a value in a given sorted list.
// if it's there, return the index of the node it is in.
// if not, return -1.
int list_lookup(struct sorted_list* list, float val) {
    struct list_node* pointer = list->head;
    int ind = 0;
    while (pointer != NULL) {
        for (int i = 0; i < pointer->num_items; i++) {
            float q = pointer->items[i];
            if (q == val) {
                return ind;
            }
        }
        pointer = pointer->next;
        ind++;
    }

    return -1;
}

void add_val(struct sorted_list* tlist, float val) {
    printf("%f\n", val);
    int add_flag = 0; // flag to add if current node is full and next is not
    struct sorted_list* list = tlist;

    if (!list->head) {
        // if list is empty, create and add a new node using val
        struct list_node* temp = new_node();
        temp->items[0] = val;
        temp->num_items++;
        list->head = temp;
        tlist = list;
        return;
    }

    
    struct list_node* temp2 = list->head;
    while (temp2 != NULL && temp2->num_items == K) {
        temp2 = temp2->next;
    }
    
    if (temp2 == NULL) {
        temp2 = new_node();
    }
    printf("here: %f\n", val);

    
    struct list_node* pointer = temp2;
    
    
    if (pointer->num_items == K) {
        // if node is full
        if (pointer->next) {
            // if pointer has a valid next node
            struct list_node* next_node = pointer->next;
            if (next_node->num_items < K) {
                add_flag = 1;
            } else {
                // if next node is full, give it a new node
                // struct list_node* temp2 = list->head;
                // while (temp2 != NULL || temp2->num_items == K) {
                //     temp2 = temp2->next;
                // }
                next_node->next = new_node();
                add_flag = 2;
            }
        }
        else {
            // if pointer has no next node
            pointer->next = new_node();
            pointer = pointer->next;
            add_flag = 2;
        }

    }

    if (add_flag == 1) {
        pointer = pointer->next;
    }
    else if (add_flag == 2) {
        pointer = pointer->next;
    }


    // addition logic
    // for (int i = 0; i < pointer->num_items; i++) {
    for (int j = pointer->num_items - 1; j >= 0; j--) {
        // float t1 = val;
        // float t2 = pointer->items[j];

        if (pointer->items[j] >= val) {
            // move val down through the array if out of order
            int ind = j;
            while (ind >= 0 && val < pointer->items[ind]) {
                pointer->items[ind + 1] = pointer->items[ind];
                pointer->items[ind] = val;
                ind--;
            }
        } else {
            pointer->items[j+1] = val;
        }
        // }
        pointer->num_items++;
        break;
    }

    tlist = list;
}

void remove_val(struct sorted_list* list, float val) {
    int del_val_flag = 0;
    int del_node_flag = 0;
    // int del_val_flag = 0;
    struct list_node* pointer = list->head;
    struct list_node* next_node = pointer->next;
    int index = list_lookup(list, val);

    if (index >= 0) { // check if the value is in the list before trying to remove it
        if (pointer->num_items == 1) {
            // if node would become empty as a result of a deletion
            del_val_flag = 1;
            del_node_flag = 1;

        }

        // delete value
        struct list_node* target = pointer->next;
        int tp = 0; // target pointer
        while (tp != index) {
            target = target->next;
        }

        int temp = 0;
        for (size_t i = 0; i < target->num_items; i++) {
            if (target->items[i] == val) {
                // delete
                for (int j = i; j < target->num_items; j++) {
                    target->items[j] = target->items[j + 1];
                }

                pointer->num_items--; // decrease amount of values in pointer

                if (del_node_flag) {
                    // get node before pointer and join it to node after pointer.
                    struct list_node* temp = list->head;
                    int tp2 = 0;
                    while (tp2 != index - 1) {
                        temp = temp->next;
                    }
                    temp->next = pointer->next;
                    // free(pointer);
                    return;
                }


                // if the current and next node have a sum of values less than K after a potential deletion, merge them
                if (pointer->num_items + next_node->num_items <= K) {
                    int pStart = pointer->num_items; // index of first non-empty value in pointer node
                    for (int i = 0; i < next_node->num_items; i++) {
                        pointer->items[pStart] = next_node->items[i];
                        pStart++;
                        pointer->num_items++;
                    }
                }

                return;
            }
        }
    }
}

void printValues(struct sorted_list* list) {
    struct list_node* node = list->head;
    while (node != NULL) {
        for (int i = 0; i < node->num_items; i++) {
            printf("%f, ", node->items[i]);
        }
        printf("\n");
        node = node->next;
    }
}

int main(int argc, char const* argv[]) {
    struct sorted_list* list = new_list();
    add_val(list, 6);
    add_val(list, 1);
    add_val(list, 2);
    add_val(list, 8);
    add_val(list, 4);
    add_val(list, 5);
    add_val(list, 3);
    add_val(list, 12);
    add_val(list, 7);
    add_val(list, 10);
    add_val(list, 9);
    add_val(list, 11);
    printValues(list);
    return 0;
}
