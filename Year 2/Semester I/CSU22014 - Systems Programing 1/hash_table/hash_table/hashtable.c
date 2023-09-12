// simple C program that contains a hash table for strings
// David Gregg, November 2020

#include "hashtable.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

// remove an item from the hash table; if the item is in the table
// multiple times, just remove the first one that we encounter
void hashtable_remove(struct hashtable* htable, char* item) {
	if (!hashtable_lookup(htable, item)) {
		return;
	}

	for (size_t i = 0; i < htable->size; i++) {
		listset_remove(htable->table[i], item);
	}

}

// print the elements of the hashtable set
void hashtable_print(struct hashtable* htable) {
	for (int i = 0; i < htable->size; i++) {
		listset_print(htable->table[i]);
	}
}
