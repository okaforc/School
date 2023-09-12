//   playing_cards.c
//   David Gregg
//   December 2020

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "playing_cards.h"

const int bits_in_byte = 8;
const int bits_in_card = 6;



// determine whether to push in a 1 or a 0
unsigned char pushIn(unsigned char c, unsigned char val, int dist);

// pack the playing card structures into bytes of memory
unsigned char* pack_playing_cards(struct playing_card* cards, int number_of_cards) {
	int maxbytes = ((number_of_cards * bits_in_card)) / bits_in_byte; // bytes taken up when packing
	unsigned char* r_cards = calloc(maxbytes, sizeof(unsigned char) * maxbytes); // packed playing cards to be returned
	int counter = 0; // for counting stored chars
	int isCarry = 0; // should i carry the some last value over?
	unsigned char carry = 0; // the value to carry over
	int howFull = 0; // how much of the bytes have been filled?
	int byteLeft = bits_in_byte; // how much of the current byte is left?
	for (int i = 0; i < number_of_cards; i++) {
		struct playing_card card = cards[i];
		if (isCarry) {
			r_cards[counter] = pushIn(r_cards[counter], carry, 6); // push in the carryover
			r_cards[counter] = pushIn(r_cards[counter], card.suit, 4); // push in the card's suit
			r_cards[counter] = pushIn(r_cards[counter], card.value, 0); // push in the card's value
			howFull += 8; // since the card and carry bits were added to this bit, it is now completely full.
			counter++; // move onto the next byte
			byteLeft = bits_in_byte; // refill the bits in the current byte
			isCarry = 0; // unset the isCarry boolean
			continue;
		}

		howFull += 2; // fill in the next two bits in the current byte. these will be taken over by the card's suit
		byteLeft -= 2; // assign the next space for the current card's suit
		r_cards[counter] = pushIn(r_cards[counter], card.suit, byteLeft);  // add the two bits of the card's suit to the next space assigned to it. 

		if (howFull % 8 == 0) {
			counter++; // move onto the next byte is the current one is filled
			byteLeft = bits_in_byte; // refill the bits in the current byte
		}
		else if (howFull % 8 == 6) {
			char temp = card.value >> 2; // create a temp char that holds the first two bits of the current card's value
			r_cards[counter] = pushIn(r_cards[counter], temp, 0); // push in the temp value
			carry = card.value & 0b00000011; // assign the last two bits as carry bits
			isCarry = 1; // as the carry bits are being used, the isCarry boolean is set
			howFull += 2; // fill in the last two bits of this byte
			counter++; // move on to the next byte
			byteLeft = bits_in_byte; // refill the bits in the current byte
			continue;
		}

		howFull += 4; // fill in the next four bits in the current byte. these will be taken over by the card's value
		byteLeft -= 4; // assign the next space for the current card's value
		r_cards[counter] = pushIn(r_cards[counter], card.value, byteLeft); // value is 4 bits, so put at pos suit - 4 = 6 - 4 = 2
	}

	for (int i = 0; i < maxbytes; i++) {
		printf("%x\n", r_cards[i]);
	}

	return r_cards;
}

unsigned char pushIn(unsigned char c, unsigned char val, int dist) {
	unsigned char c1 = val;
	unsigned char c2 = c;
	c1 = c1 << dist;
	
	return (unsigned char)(c1 | c2);
}


// unpack bytes of memory containing card data into playing card structures
struct playing_card* unpack_playing_cards(unsigned char* packed_cards, int number_of_cards) {
	struct playing_card* cards = calloc(number_of_cards, sizeof(struct playing_card));
	return cards;
}
