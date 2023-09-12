#include <stdlib.h>
#include <string.h>
#include <assert.h>

struct Array
{
    int *array; // the array
    int size; // the size of the array
    int limit; // the limit of the array. if the limit is reached, more space to the array.
};

// Instantiate a new Array of size s
struct Array* newArray(int s) {
    struct Array* arr = malloc(sizeof(struct Array));
    arr->array = calloc(s, sizeof(int));
    arr->limit = s;
    arr->size = 0;
    return arr;
}

// Append a new element val to an array arr
void appendArray(struct Array *arr, int val) {
    // if the array would overflow upon adding a new element,
    if (arr->limit == arr->size) {
        if (arr->limit == 0) {
            arr->limit = 1;
        }
        arr->limit *= 2; // increase the size of the array by 1
        arr->array = (int*)realloc(arr->array, arr->limit * sizeof(int)); // reallocate the space of the array
        int i;
        for (i = arr->size; i < arr->limit; i++) {
            arr->array[i] = 0;
        }
    }

    arr->array[arr->size++] = val; // add the new element at the old limit.
}

// Remove and return the last value in arr
int popArray(struct Array *arr) {
    assert(arr->limit > 0); // crash if array becomes too small

    // if the array would become too small after popping
    if (arr->size < arr->limit/2) {
        arr->limit /= 2; // halve the size of the array
        arr->array = (int*)realloc(arr->array, arr->limit * sizeof(int)); // reallocate the space of the array
        int i;
        for (i = arr->size; i < arr->limit; i++) {
            arr->array[i] = 0;
        }
    }

    
    return arr->array[arr->size--]; // return the value at the end and exit
}

// Get the value of the array arr at the index ind
int getArrayValue(struct Array* arr, int ind) {
    assert(ind < arr->size);
    assert(ind > -1);
    
    if (ind < arr->size) {
        return arr->array[ind];
    } else {
        return -1; // this should never be returned. use the code properly.
    }
}

// Reset an array arr given its address
void freeArray(struct Array *arr) {
    free(arr);
    arr->array = NULL;
    arr->limit = 0;
    arr->size = 0;    
}