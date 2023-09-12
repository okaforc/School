#include "sort-harness.h"
#include "student_sort.h"
#include <x86intrin.h>

/* create an array of length size and fill it with random numbers between `start` (inclusive) and `end` (exclusive) */
float *gen_rand_range(int size, int start, int end)
{
	float *result = malloc(sizeof(float) * size);
	int i;

	/* fill the array with random numbers */
	for (i = 0; i < size; i++)
	{
		int v = start+random()%end; // random number between start and end (inclusive)
		result[i] = (int)(v);
	}

	return result;
}

/* given an array of length size, select n random values and return the selection as an array */
float *select_random(float *arr, int size, int n)
{
	float *result = malloc(sizeof(float) * n);
	float *indexes = gen_rand_range(n, 0, size); // generate a random spread of valid indexes

	/* fill the array with random numbers correspoding to above indexes */
	for (int i = 0; i < n; i++) {
		result[i] = arr[(int)indexes[i]];
	}

	return result;
}


/* Calcualte the hamming weight of an int */
int hamming_weight(uint32_t i) {
	// using bit hacks
	i = i - ((i >> 1) & 0x55555555);
	i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
	i = (i + (i >> 4)) & 0x0F0F0F0F;
	return (i * 0x01010101) >> 24;
}

/* Insertion sort */
void insertion_sort(float a[], int size) {
	int i = 1;
	while (i < size) {
		float temp = a[i];
		int j = i - 1;
		while (j >= 0 && a[j] > temp) {
			a[j+1] = a[j];
			j--;
		}
		a[j+1] = temp;
		i++;
	}
}
/* Print out all 4 lanes of an m128 vector */
void printVector(__m128 in) {
    float* v = malloc(sizeof(float)*4);
    _mm_storeu_ps(v, in);
    printf("val: %.f %.f %.f %.f\n", v[0], v[1], v[2], v[3]);
}

/* Find the index of the first zero in the given m128 vector, or return -1 if there is none */
int zeroInVector(__m128 v) {
	float* vs = malloc(sizeof(float)*4);
    _mm_storeu_ps(vs, v);
	if (vs[0] == 0) return 0;
	else if (vs[1] == 0) return 1;
	else if (vs[2] == 0) return 2;
	else if (vs[3] == 0) return 3;
	else return -1;
}

// int llla = 0, lllb = 0;
// https://www.inf.hs-flensburg.de/lang/algorithmen/sortieren/bitonic/oddn.htm
void bitonic_merge(float a[], int size, int lo, int dir) {
	if (size > 1) {
		// find the highest power of 2 less than size
		int hp = 1;
		while (hp > 0 && hp < size) {
			hp = hp << 1;
		}
		hp = hp >> 1;

		int i = 0;
		int ts = lo + size - hp; // temp size
		int remainder = ts % 4;
		if (hp >= 4) {
			for (; i < ts - remainder; i += 4) {
				// llla++; // sse swapping
				__m128 ma = _mm_loadu_ps(&(a[i]));
				__m128 mb = _mm_loadu_ps(&(a[i+hp]));
				__m128 mmin = _mm_min_ps(ma, mb);
				__m128 mmax = _mm_max_ps(ma, mb);
				if (dir) {
					// sort ascending
					_mm_storeu_ps(&(a[i]), mmin);
					_mm_storeu_ps(&(a[i+hp]), mmax);
				} else {
					// sort descending
					_mm_storeu_ps(&(a[i]), mmax);
					_mm_storeu_ps(&(a[i+hp]), mmin);
				}
				
				bitonic_merge(a, hp, lo, dir);
				bitonic_merge(a, size - hp, lo + hp, dir);
			}
		}
		// remaining values
		for (; i < ts; i++) {
			// lllb++; // standard swapping
			if (dir == (a[i] > a[i+hp])) {
				float tmp = a[i];
				a[i] = a[i+hp];
				a[i+hp] = tmp;
			}
			bitonic_merge(a, hp, lo, dir);
			bitonic_merge(a, size - hp, lo + hp, dir);
		}
	}
}


void bitonic_helper(float a[], int size, int lo, int dir) {
	if (size > 1) {
		int m = size/2;
		// form the bitonic sequence by sorting one half ascending and the other descending
		bitonic_helper(a, m, lo, !dir);
		bitonic_helper(a, size-m, lo+m, dir);
		bitonic_merge(a, size, lo, dir);
	}
}

void bitonic_sort(float a[], int size) {
	bitonic_helper(a, size, 0, 1);
	// printf("sse: %d\nstandard: %d\n", llla, lllb);
}

/* Sample sort helper (SSE) */
float *sample_sort_helper(float a[], int size) {
	int nb = 5; // no. buckets
	int oversample = 1;
	int nspn = nb/oversample;
	if (size/oversample < 20) {
		// use insertion sort when under threshold
		insertion_sort(a, size);
		return a;
	}
	int sn = (nb-1)*oversample; // size of sample
	float *samples = select_random(a, size, sn); // generate samples
	insertion_sort(samples, sn); // sort samples
	float *splitters = malloc(sizeof(float) * nspn); // create array to hold splitters
	for (int i = 0; i < sn; i += oversample) {
		// get every `oversample`th value and use those as splitters
		splitters[i] = samples[i];
	}

	int spln = sn/oversample; // number of splitters
	
	__m128 vsplitters = _mm_loadu_ps(splitters); // load splitters into vector
	__m128 val;
	
	float **buckets = calloc(nb, sizeof(float*)); // buckets to sort values later
	float **ebcks = calloc(nb, sizeof(float*)); // equality buckets, which aren't sorted
	for (int i = 0; i < nb; i++) {
		buckets[i] = calloc(2, sizeof(float));
		ebcks[i] = calloc(2, sizeof(float));
	}
	int *sizes = calloc(1, sizeof(int)); // sizes of each bucket
	int *caps = calloc(nb, sizeof(int)); // capacities of each bucket
	int *esizes = calloc(1, sizeof(int)); // sizes of each equality bucket
	int *ecaps = calloc(nb, sizeof(int)); // capacities of each equality bucket

	for (int i = 0; i < size; i++) {
		int flag = 1;
		val = _mm_set1_ps(a[i]); // load the current value into all 4 lanes of vector
		__m128 sub_res = _mm_sub_ps(vsplitters, val); // sign mask of subtraction 
		int zidx = zeroInVector(sub_res); // get the index of the first zero in sub_res

		if (zidx == -1) {
			int sub_res_mask = _mm_movemask_ps(sub_res);
			int sbits = hamming_weight(sub_res_mask); // number of set bits in sign mask
			if (sizes[sbits] >= caps[sbits]) {
				// if capacity is breached, increase the space of this bucket
				buckets[sbits] = (float*)realloc(buckets[sbits], sizeof(float)*(caps[sbits]+4));
				caps[sbits]+=4;
			}
			buckets[sbits][sizes[sbits]] = a[i]; // load value into specified bucket
			sizes[sbits]++; // increase size of this bucket
		} else {
			// if the value is equal to a splitter, add it to an equality bucket instead
			if (esizes[zidx] >= ecaps[zidx]) {
				// if capacity is breached, increase the space of this bucket
				ebcks[zidx] = (float*)realloc(ebcks[zidx], sizeof(float)*(ecaps[zidx]+4));
				ecaps[zidx]+=4;
			}
			ebcks[zidx][esizes[zidx]] = a[i]; // load value into specified bucket
			esizes[zidx]++; // increase size of this bucket
		}
	}

	float *ans = malloc(sizeof(float)*(size));
	float *tarr = malloc(sizeof(float)*1);
	float *etarr = malloc(sizeof(float)*1);
	int p = 0;
	
	for (int i = 0; i < nb; i++) {
		tarr = sample_sort_helper(buckets[i], sizes[i]); // current bucket
		int j, remainder = sizes[i] % 4; // bucket index j, remainder
		for (j = 0; j < sizes[i] - remainder; j += 4) {
			__m128 t4 = _mm_loadu_ps(&(tarr[j])); // load vector from bucket
			_mm_storeu_ps(&(ans[p]), t4); // store values in this vector to the answer
			p += 4; // iterate through answer array
		}

		// for any leftover values outside the remainder
		for (; j < sizes[i]; j++) {
			ans[p] = tarr[j];
			p++;
		}
		
		// equality buckets are all already sorted on account of being the same duplicate
		etarr = ebcks[i]; // current equality bucket
		remainder = esizes[i] % 4; // bucket index j, remainder
		for (j = 0; j < esizes[i] - remainder; j += 4) {
			__m128 t4 = _mm_loadu_ps(&(etarr[j])); // load vector from bucket
			_mm_storeu_ps(&(ans[p]), t4); // store values in this vector to the answer
			p += 4; // iterate through answer array
		}

		// for any leftover values outside the remainder
		for (; j < esizes[i]; j++) {
			ans[p] = etarr[j];
			p++;
		}
	}

	// free all allocated space
	while (nb > 0) {
		nb--;
		free(buckets[nb]);
		free(ebcks[nb]);
	}
	// free(tarr); // causes a segfault, don't know why
	free(buckets);
	free(sizes);
	free(caps);
	free(ebcks);
	free(esizes);
	free(ecaps);
	return ans;
}


/* Sample sort */
void sample_sort(float a[], int size) {
	float *tmp = sample_sort_helper(a, size);
	memcpy(a, tmp, sizeof(float)*size);
}



void student_sort(float a[], int size) {
	// bitonic_sort(a, size);
	sample_sort(a, size);
	// david_sort(a, size);
}

