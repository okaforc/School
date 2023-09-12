//#ifndef STUDENT_SORT_H
//#define STUDENT_SORT_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/time.h>
#include <x86intrin.h>
void student_sort(float a[], int size);
void sample_sort(float a[], int size);
void insertion_sort(float a[], int size);
float *sample_sort_helper(float a[], int size);

void bitonic_helper(float a[], int size, int lo, int dir);
void bitonic_merge(float a[], int size, int lo, int dir);
void bitonic_sort(float a[], int size);

int float_compare(const void *ptr2num1, const void *ptr2num2);
float *gen_random(int size); 
float *gen_rand_range(int size, int start, int end);
float *select_random(float *arr, int size, int n);
int hamming_weight(uint32_t i);
// int verify(float* a, int size);
int zeroInVector(__m128 v);
//#endif
