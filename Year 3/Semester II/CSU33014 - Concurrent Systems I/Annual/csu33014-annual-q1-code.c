//
// CSU33014 Annual Exam, April 2023
// Question 1
//

// Please examine version each of the following routines with names
// starting 'routine_'. Where the routine can be vectorized, please
// replace the corresponding 'vectorized' version using SSE vector
// intrinsics. Where it cannot be vectorized please explain why.

// To illustrate what you need to do, routine_0 contains a
// non-vectorized piece of code, and vectorized_0 shows a
// corresponding vectorized version of the same code.

// Note that to simplify testing, I have put a copy of the original
// non-vectorized code in the vectorized version of the code for
// routines 1 to 6. This allows you to easily see what the output of
// the program looks like when the original and vectorized version of
// the code produce equivalent output.

// Note the restrict qualifier in C indicates that "only the pointer
// itself or a value directly derived from it (such as pointer + 1)
// will be used to access the object to which it points".

#include "csu33014-annual-q1-code.h"

#include <immintrin.h>
#include <stdio.h>
/* Print out all 4 lanes of an m128 vector */
void printVector(__m128 in) {
    float* v = malloc(sizeof(float) * 4);
    _mm_storeu_ps(v, in);
    printf("%.f %.f %.f %.f\n", v[3], v[2], v[1], v[0]);
}
/****************  routine 0 *******************/

// Here is an example routine that should be vectorized
void Q1_routine_0(float* restrict a, float* restrict b, float* restrict c) {
    for (int i = 0; i < 1024; i++) {
        a[i] = b[i] * c[i];
    }
}

// here is a vectorized solution for the example above
void Q1_vectorized_0(float* restrict a, float* restrict b, float* restrict c) {
    __m128 a4, b4, c4;

    for (int i = 0; i < 1024; i = i + 4) {
        b4 = _mm_loadu_ps(&b[i]);
        c4 = _mm_loadu_ps(&c[i]);
        a4 = _mm_mul_ps(b4, c4);
        _mm_storeu_ps(&a[i], a4);
    }
}

/***************** routine 1 *********************/

// in the following, size can have any positive value
void Q1_routine_1(float* restrict a, float* restrict b, int size) {
    for (int i = 0; i < size; i++) {
        a[i] = 3.73892 + (b[i] * 3.73892);
    }
}

// in the following, size can have any positive value
void Q1_vectorized_1(float* restrict a, float* restrict b, int size) {
    __m128 av, bv,
        act;  // vector for a, vector for b, vector for actor (3.73892)
    act = _mm_set1_ps(3.73892);  // set all four lanes to the actor value
    int vsize = size - 3, i;
    for (i = 0; i < vsize; i += 4) {
        av = _mm_add_ps(act, _mm_mul_ps(_mm_loadu_ps(&(b[i])), act));
        _mm_storeu_ps(&(a[i]), av);
    }
    for (; i < size; i++) {
        a[i] = 3.73892 + (b[i] * 3.73892);
    }
}

/******************* routine 2 ***********************/

// in the following, size can have any positive value
float Q1_routine_2(float* restrict a, int size) {
    float x = 0.0;
    float y = 0.0;

    for (int i = 0; i < size - 1; i++) {
        x = x + a[i];
        y = y + a[i + 1];
    }
    return x + y * 10.0;
}

// in the following, size can have any positive value
float Q1_vectorized_2(float* restrict a, int size) {
    float x = 0.0;
    float y = 0.0;
    __m128 xv = _mm_setzero_ps();
    int vsize = size - 3, i;
    for (i = 0; i < vsize; i += 4) {
        xv = _mm_add_ps(xv, _mm_loadu_ps(&(a[i])));
    }
    float* tmp = malloc(sizeof(float) * 4);
    _mm_storeu_ps(tmp, xv);
    x = tmp[0] + tmp[2];
    y = tmp[1] + tmp[3];

    for (; i < size - 1; i++) {
        x = x + a[i];
        y = y + a[i + 1];
        printf("got here\n");
    }
    return x + y * 10.0;
}

/******************** routine 3 ************************/

// in the following, size can have any positive value
void Q1_routine_3(float* restrict a, float* restrict b, int size) {
    for (int i = 0; i < size; i++) {
        if (a[i] > (b[i] + 1.0)) {
            float temp = a[i];
            a[i] = b[i];
            b[i] = temp;
        }
    }
}

// in the following, size can have any positive value
void Q1_vectorized_3(float* restrict a, float* restrict b, int size) {
    // vector for a, vector for b, result vector, min vector, max vector, vector
    // for actor (1.0)
    __m128 av, bv, res1, res2, res3, vmin, vmax, act = _mm_set1_ps(1.0);
    int vsize = size - size % 4, i;
    for (i = 0; i < vsize; i += 4) {
        av = _mm_loadu_ps(&(a[i]));
        bv = _mm_loadu_ps(&(b[i]));
        res1 = _mm_add_ps(bv, act);   // add 1.0 to b[i]
        vmin = _mm_min_ps(av, res1);  // get min of a and b+1
        vmax = _mm_max_ps(av, res1);  // get max of a and b+1
        res2 = _mm_div_ps(_mm_and_ps(av, _mm_cmpgt_ps(av, res1)), av);  // get mask of greater than values
        res3 = _mm_div_ps(_mm_and_ps(av, _mm_cmple_ps(av, res1)), av);  // get last of less than/equal to values
        _mm_storeu_ps(&(a[i]), _mm_sub_ps(vmin, res2));  // remove the b+1 when storing
        _mm_storeu_ps(&(b[i]), _mm_sub_ps(vmax, res3));  // remove the b+1 when storing
    }

    for (; i < size; i++) {
        if (a[i] > (b[i] + 1.0)) {
            float temp = a[i];
            a[i] = b[i];
            b[i] = temp;
        }
    }
}

/********************* routine 4 ***********************/

void Q1_routine_4(float* restrict a) {
    for (int i = 0; i < 2048; i = i + 2) {
        float temp = a[i];
        a[i] = a[i + 1];
        a[i + 1] = temp;
    }
}

void Q1_vectorized_4(float* a) {
    // vector for a, vector for b, result vector, min vector, max vector, vector
    // for actor (1.0)
    __m128 av, sv;
    int i;
    for (i = 0; i < 2048; i += 4) {
        av = _mm_loadu_ps(&(a[i]));  // load values
        av = _mm_shuffle_ps(
            av, av,
            _MM_SHUFFLE(2, 3, 0, 1));  // shuffle values by swapping every second value
        _mm_storeu_ps(&(a[i]), av);
    }
}

/********************* routine 5 ***********************/

// in the following, size can have any positive value
void Q1_routine_5(unsigned char* restrict a, unsigned char* restrict b,
                  unsigned char* restrict c, int size) {
    for (int i = 1; i < size; i++) {
        a[i - 1] = b[i + 1] ^ c[i - 1];
    }
}

// in the following, size can have any positive value
void Q1_vectorized_5(unsigned char* restrict a, unsigned char* restrict b,
                     unsigned char* restrict c, int size) {
    for (int i = 1; i < size; i++) {
        a[i - 1] = b[i + 1] ^ c[i - 1];
    }
}

/********************* routine 6 ***********************/

// in the following size is a positive value that is a multiple of 3
float Q1_routine_6(float* restrict a, float* restrict b, float* restrict c) {
    for (int i = 0; i < 1024; i++) {
        float x = a[i];
        float y = b[i];
        c[i] = 0;
        for (int j = 0; j < 100; j++) {
            x = (x * x) - (y * y);
            y = x * y * 2;
            if ((x > 1000.0) || (y > 1000.0)) {
                c[i] = 1;
                break;
            }
        }
    }
}

// in the following size is a positive value that is a multiple of 3
float Q1_vectorized_6(float* restrict a, float* restrict b, float* restrict c) {
    for (int i = 0; i < 1024; i++) {
        float x = b[i];
        float y = c[i];
        a[i] = 0;
        for (int j = 0; j < 100; j++) {
            x = (x * x) - (y * y);
            y = x * y * 2;
            if ((x > 1000.0) || (y > 1000.0)) {
                a[i] = 1;
                break;
            }
        }
    }
}