//   playing_cards.c
//   David Gregg
//   December 2020

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "playing_cards.h"

const int bits_in_byte = 8;
const int bits_in_card = 6;
const int cards_in_pack = 52;


// determine whether to push in a 1 or a 0
unsigned char pushIn(unsigned char c, unsigned char val, int dist);

// get a certain range of bits from a byte
unsigned char getBits(unsigned char c, int start, int dist);

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

	/* for (int i = 0; i < maxbytes; i++) {
		printf("%x\n", r_cards[i]);
	} */

	return r_cards;
}

unsigned char pushIn(unsigned char c, unsigned char val, int dist) {
	unsigned char c1 = val;
	unsigned char c2 = c;
	c1 = c1 << dist;
	
	return (unsigned char)(c1 | c2);
}

unsigned char getBits(unsigned char c, int start, int dist) {
	// create a mask the length of dist. this is done by creating shifting 1 to the right dist times,
	// and then subtracting 1.
	// example:
	//	getBits(164, 1, 6); -> 164 = 10100100
	//	==> p = 1010010
	//	==> mask = 1000000 - 1 => 111111
	// 	==> return 1010010 & 111111 => 010010 => 18

	unsigned char init = c >> start; // shift char to end to eliminate any bits at the start
	unsigned char mask = (1 << dist) - (unsigned char)1;
	return mask & init;
}

// unpack bytes of memory containing card data into playing card structures
struct playing_card* unpack_playing_cards(unsigned char* packed_cards, int number_of_cards) {
	struct playing_card* cards = calloc(number_of_cards, sizeof(struct playing_card));
	int npacked = ((number_of_cards * bits_in_card)) / bits_in_byte; // number of packed cards

	int counter = 0; // for counting stored chars
	int isCarry = 0; // should i carry the some last value over?
	unsigned char carry = 0; // the value to carry over
	int howFull = 0; // how much of the cards have been filled?
	int byteLeft = bits_in_byte; // how much of the current byte is left?
	int lastSuit = 0; // was the last suit assigned?

	for (int i = 0; i < npacked; i++) {
		unsigned char c = packed_cards[i];
		byteLeft = bits_in_byte;

		if (isCarry) {
			carry = pushIn(carry, getBits(c, 6, 2), 0);
			cards[counter].value = carry;
			byteLeft -= 2;
			howFull += 2;
			isCarry = 0;
			counter++;
		}

		if (!lastSuit) {
			byteLeft -= 2;
			howFull += 2;
			cards[counter].suit = getBits(c, byteLeft, 2); // get bits 6 and 7
		}
		else {
			lastSuit = 0;
		}

		byteLeft -= 4;
		howFull += 4;
		cards[counter].value = getBits(c, byteLeft, 4); // get bits 2 - 5 incl.
		counter++;

		if (byteLeft == 4) {
			cards[counter].suit = getBits(c, 2, 2);
			carry = pushIn((unsigned char)0, getBits(c, 0, 2), 2);
			isCarry = 1;
			byteLeft -= 2;
			howFull += 2;
			continue;
		}
		else if (byteLeft == 2) {
			cards[counter].suit = getBits(c, 0, 2);
			lastSuit = 1;
		}
	}

	return cards;
}
