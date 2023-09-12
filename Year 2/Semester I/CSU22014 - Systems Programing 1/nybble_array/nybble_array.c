//   nybble_array.c
//   David Gregg
//   December 2021

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "nybble_array.h"

// get a certain range of bits from a byte, starting from the rightmost (least significant) bit.
unsigned char getBits(unsigned char c, int start, int dist);

// push in a value into a byte in a range of bits, starting at a given pos from the rightmost bit.
unsigned char pushIn(unsigned char c, unsigned char val, int pos);

// create a new array of nybbles with space for "size"
// nybbles and initialize the values of the array to zero
struct nybble_array* nybble_array_new(int size) {
	struct nybble_array* newarr = malloc(sizeof(struct nybble_array)); // allocate memory for the structure
	newarr->size_in_nybbles = size; // set the size of nybbles
	newarr->size_in_bytes = size / 2; // set the size of bytes. since 2 nybbles fit into 1 byte, newarr needs size/2 bytes.
	newarr->data_bytes = calloc(size, sizeof(unsigned char) * size); // allocate and clear memory for the data bytes
	return newarr;
}

// return the nybble value at position index
unsigned get_nybble_value(struct nybble_array* this, int index) {
	int isOdd = index % 2; // boolean to check if the index is odd or even. returns 1 if odd and 0 if not.
	int byteIndex = index / 2; // index of the byte where the nybble lies.
	unsigned char cByte = this->data_bytes[byteIndex]; // the byte where the desired nybble is
	if (isOdd) {
		return getBits(cByte, 0, 4);
	}
	else {
		return getBits(cByte, 4, 4);
	}
}

// set the nybble at position index to value
void set_nybble_value(struct nybble_array* this, int index, unsigned value) {
	int isOdd = index % 2; // boolean to check if the index is odd or even. returns 1 if odd and 0 if not.
	int byteIndex = index / 2; // index of the byte where the nybble lies.
	if (isOdd) {
		this->data_bytes[byteIndex] = pushIn(this->data_bytes[byteIndex], value, 0); // push the given value into the data byte at the index 0
	}
	else {
		this->data_bytes[byteIndex] = pushIn(this->data_bytes[byteIndex], value, 4);
	}
}

// free the memory used by a nybble array
void nybble_array_free(struct nybble_array* this) {
	free(this);
}

// given an array of unsigned integers with values 0 to 15 create
// a new nybble_array with the same values
struct nybble_array* unsigned_to_nybble_array(unsigned* array, int size) {
	struct nybble_array* arr = nybble_array_new(size * 2); // initialise a new nybble array

	// loop through both the unsigned array and the nybble array using a second pointer, setting the nybble values
	int nybblePointer = 0;
	for (int j = 0; j < size; j++) {
		arr->data_bytes[j] = pushIn(arr->data_bytes[j], array[nybblePointer], 4); // set the last 4 bits to the current array value
		nybblePointer++; // increment the nybblePointer counter
		arr->data_bytes[j] = pushIn(arr->data_bytes[j], array[nybblePointer], 0); // set the first 4 bits to the current array value
		nybblePointer++;
	}

	return arr;
}

// given an array of nybbles, create a new array of unsigned integers
// with the same values
unsigned* nybble_array_to_unsigned(struct nybble_array* this) {
	unsigned* array = calloc(this->size_in_nybbles, sizeof(unsigned) * this->size_in_nybbles);

	int innerPointer = 0;
	int start = 4; // start position to be used in getBits

	// flag to check if i should take the first or last (rightmost or leftmost) four bits from the data bytes
	// 1 = rightmost / least significant bit
	// 0 = leftmost / most significant bit
	int rightOrLeft = 0;
	for (int i = 0; i < this->size_in_nybbles; i++) {
		if (rightOrLeft == 1) {
			start = 0;
			rightOrLeft = 0;
			// this is taking the rightmost four bits. that means that we're at the end of the current data byte
			// and should move on to the next one.
			innerPointer++;
		}
		else {
			start = 4;
			rightOrLeft = 1;
		}

		array[i] = getBits(this->data_bytes[innerPointer], start, 4); // assign a new array value whichever half of the data byte we're on
	}

	return array;
}

// print the raw byte content of the nybble array
void print_raw_bytes_nybble_array(struct nybble_array* this) {
	for (int i = 0; i < this->size_in_bytes; i++) {
		printf("%x ", this->data_bytes[i]);
	}
	printf("\n");
}

unsigned char getBits(unsigned char c, int start, int dist) {
	// create a mask the length of dist. this is done by creating shifting 1 to the right dist times,
	// and then subtracting 1.
	// example:
	//	getBits(164, 1, 6); // 164 = 10100100
	//	==> init = c >> start = 10100100 >> 1 = 1010010
	//	==> mask = 1000000 - 1 => 111111
	// 	==> return 1010010 & 111111 => 010010 => 18

	unsigned char init = c >> start; // shift char to end to eliminate any bits before start
	unsigned char mask = ((unsigned char)1 << dist) - (unsigned char)1; // create the mask to get only the desired bits
	return mask & init;
}

unsigned char pushIn(unsigned char c, unsigned char val, int pos) {
	unsigned char c1 = val;
	unsigned char c2 = c;
	c1 = c1 << pos;

	return c1 | c2;
}
