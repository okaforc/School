#include "student_sort.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <x86intrin.h>

/******************* routine 1 ***********************/
void scale(float *array, float factor, float offset)
{
    // for (int i = 0; i < 1024; i++)
    // {
    //     array[i] = (array[i] * factor) + offset;
    // }

    __m128 factor4 = _mm_set1_ps(factor);
    __m128 offset4 = _mm_set1_ps(offset);
    for (int i = 0; i < 1024; i = i + 4)
    {
        __m128 array4 = _mm_loadu_ps(&(array[i]));
        __m128 product = _mm_mul_ps(array4, factor4);
        __m128 result = _mm_add_ps(product, offset4);
        _mm_storeu_ps(&(array[i]), result);
    }
}

/******************* routine 2 ***********************/

/* code segment 2 */
float sum(float *a, int size)
{
    // float sum = 0.0;
    // for (int i = 0; i < size; i++)
    // {
    //     sum = sum + a[i];
    // }
    // return sum;

    __m128 sum4 = _mm_setzero_ps();
    int remainder = size % 4;
    int i;
    for (i = 0; i < size - remainder; i = i + 4)
    {
        __m128 a4 = _mm_loadu_ps(&(a[i]));
        sum4 = _mm_add_ps(sum4, a4);
    }
    // add the four partial sums
    float temp[4];
    _mm_storeu_ps(temp, sum4);
    float sum = temp[0] + temp[1] + temp[2] + temp[3];

    // add remainder iterations
    for (; i < size; i++)
    {
        sum = sum + a[i];
    }
    return sum;
}

/******************* routine 3 ***********************/

// in the following, size can have any positive value
float routine_3(float *restrict a, float *restrict b, int size)
{
    float sum_a = 0.0;
    float sum_b = 0.0;

    for (int i = 0; i < size; i++)
    {
        sum_a = sum_a + a[i];
        sum_b = sum_b + b[i];
    }
    return sum_a * sum_b;
}

/******************* routine 4 ***********************/

// in the following, size can have any positive value
void routine_4(float *restrict a, float *restrict b, int size)
{
    for (int i = 0; i < size; i++)
    {
        a[i] = 1.5379 - (1.0 / b[i]);
    }
}

/******************** routine 5 ************************/

// in the following, size can have any positive value
void routine_5(float *restrict a, float *restrict b, int size)
{
    // for (int i = 0; i < size; i++)
    // {
    //     if (a[i] < b[i])
    //     {
    //         a[i] = b[i];
    //     }
    // }

    int remainder = size % 4;
    int i;
    for (i = 0; i < size - remainder; i = i + 4)
    {
        __m128 a4 = _mm_loadu_ps(&(a[i]));
        __m128 b4 = _mm_loadu_ps(&b[i]);
        __m128 max4 = _mm_max_ps(a4, b4);
        _mm_storeu_ps(&(a[i]), max4);
    }
    for (; i < size; i++)
    {
        if (a[i] < b[i])
            a[i] = b[i];
    }
}

/********************* routine 6 ***********************/

void multiply(float **matrix, float *vec, float *result)
{
    for (int i = 0; i < 4096; i++)
    {
        //     float sum = 0.0;
        //     for (int j = 0; j < 4096; j++)
        //     {
        //         sum += vec[j] * matrix[i][j];
        //     }
        //     result[i] = sum;

        __m128 sum4 = _mm_setzero_ps();
        for (int j = 0; j < 4096; j = j + 4)
        {
            // sum += vec[j] * matrix[i][j];
            __m128 vec4 = _mm_loadu_ps(&(vec[i]));
            __m128 mat4 = _mm_loadu_ps(&(matrix[i][j]));
            __m128 product4 = _mm_mul_ps(vec4, mat4);
            sum4 = _mm_add_ps(sum4, product4);
        }
        // sum the four partial products
        float temp[4];
        _mm_storeu_ps(temp, sum4);
        result[i] = temp[0] + temp[1] + temp[2] + temp[3];
    }
}

/********************* routine 7 ***********************/

// hint: one way to vectorize the following code might use
// vector shuffle operations
void routine_7(float *restrict a, float *restrict b,
               float *restrict c)
{
    for (int i = 0; i < 2048; i = i + 2)
    {
        a[i] = b[i] * c[i + 1] + b[i + 1] * c[i];
        a[i + 1] = b[i] * c[i] - b[i + 1] * c[i + 1];
    }
}

/********************* routine 8 ***********************/

// in the following, size can have any positive value
int routine_8(unsigned char *restrict a, unsigned char *restrict b, int size)
{
    for (int i = 0; i < size; i++)
    {
        if (a[i] != b[i])
            return 0;
    }
    return 1;
}

/********************* routine 9 ***********************/

void routine_6(float *restrict a, float *restrict b, float *restrict c)
{
    a[0] = 0.0;
    for (int i = 1; i < 1023; i++)
    {
        float sum = 0.0;
        for (int j = 0; j < 3; j++)
        {
            sum = sum + b[i + j - 1] * c[j];
        }
        a[i] = sum;
    }
    a[1023] = 0.0;
}
