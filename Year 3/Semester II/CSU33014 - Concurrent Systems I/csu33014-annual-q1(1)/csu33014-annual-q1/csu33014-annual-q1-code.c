//
// CSU33014 Annual Exam, May 2022
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
    float* v = malloc(sizeof(float)*4);
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
float Q1_routine_1(float* restrict a, float* restrict b, int size) {
    float product_a = 1.0;
    float product_b = 1.0;

    for (int i = 0; i < size; i++) {
        // fprintf(stderr, "pa %f, ai %f, pb %f, bi %f\n", product_a, a[i],
        // product_b, b[i]);
        product_a = product_a * a[i];
        product_b = product_b * b[i];
    }
    return product_a + product_b;
}

// in the following, size can have any positive value
float Q1_vectorized_1(float* restrict a, float* restrict b, int size) {
    float product_a = 1.0;
    float product_b = 1.0;
	__m128 av, bv;
	int vsize = size - size%4, i; // size mod 4, for vectors
	av = _mm_loadu_ps(&a[0]); // load in first 4 values from array
	bv = _mm_loadu_ps(&b[0]); // load in first 4 values from array

    for (i = 4; i < vsize; i+=4) {
		av = _mm_mul_ps(av, _mm_loadu_ps(&a[i]));
		bv = _mm_mul_ps(bv, _mm_loadu_ps(&b[i]));
    }

	float* atmp = malloc(sizeof(float)*4); // temp array
	float* btmp = malloc(sizeof(float)*4); // temp array
	_mm_storeu_ps(atmp, av);
	_mm_storeu_ps(btmp, bv);
	product_a *= atmp[0]*atmp[1]*atmp[2]*atmp[3];
	product_b *= btmp[0]*btmp[1]*btmp[2]*btmp[3];

	for ( ; i < size; i++) {
        product_a = product_a * a[i];
        product_b = product_b * b[i];
    }

    return product_a + product_b;
}

/******************* routine 2 ***********************/

// in the following, size can have any positive value
void Q1_routine_2(float* restrict a, float* restrict b, int size) {
    for (int i = 0; i < size; i++) {
        a[i] = (a[i] * 17.2) + (3.14159 / b[i]);
    }
}

// in the following, size can have any positive value
void Q1_vectorized_2(float* restrict a, float* restrict b, int size) {
	// load vectors for arrays a and b, and the actors for each
	__m128 av, bv, aact = _mm_set1_ps((float)17.2), bact = _mm_set1_ps((float)3.14159);
	int vsize = size - size%4, i;
    for (i = 0; i < vsize; i+=4) {
		av = _mm_mul_ps(_mm_loadu_ps(&(a[i])), aact);
		bv = _mm_div_ps(bact, _mm_loadu_ps(&(b[i])));
		_mm_storeu_ps(&a[i], _mm_add_ps(av, bv));
    }
    for ( ; i < size; i++) {		
        a[i] = (a[i] * 17.2) + (3.14159 / b[i]);
    }
}

/******************** routine 3 ************************/

// in the following, size can have any positive value
void Q1_routine_3(float* restrict a, float* restrict b, int size) {
    for (int i = 0; i < size; i++) {
        if (a[i] > b[i]) {
            a[i] = -a[i];
        }
    }
}

// in the following, size can have any positive value
void Q1_vectorized_3(float* restrict a, float* restrict b, int size) {
	// load vectors for arrays a and b
	__m128 av, bv;
	int vsize = size - size%4, i;
    for (i = 0; i < vsize; i+=4) {
		av = _mm_loadu_ps(&(a[i]));
		bv = _mm_loadu_ps(&(b[i]));
		__m128 res = _mm_cmpgt_ps(av, bv); // get greater-than signmask of av and bv

		// for any x, x - 2x = -x.
		// here, we AND av with the signmask and multiply it by 2 to get all 2x values and zeroes everywhere else
		// 2x is then subtracted from av (x) to get -x
		av = _mm_sub_ps(
			av,
			_mm_mul_ps(
				_mm_and_ps(av, res),
				_mm_set1_ps(2)
			)
		);
		_mm_storeu_ps(&(a[i]), av); // write to array
    }
	// _mm_shuffle_ps(av, bv, _MM_SHUFFLE(3, 0, 2, 1));

	// rest
    for ( ; i < size; i++) {
        if (a[i] > b[i]) {
            a[i] = -a[i];
        }
    }
}

/********************* routine 4 ***********************/

void Q1_routine_4(float* restrict a, float* restrict b, float* restrict c) {
    for (int i = 0; i < 2048; i++) {
        if (i == 0) {
            a[i] = b[i] * c[i];
        } else if (i == 2047) {
            a[i] = b[i] * c[i];
        } else {
            a[i] = b[i - 1] * c[i + 1] + b[i] * c[i] + b[i + 1] * c[i - 1];
        }
    }
}

void Q1_vectorized_4(float* restrict a, float* restrict b, float* restrict c) {
    for (int i = 0; i < 2048; i++) {
        if (i == 0) {
            a[i] = b[i] * c[i];
        } else if (i == 2047) {
            a[i] = b[i] * c[i];
        } else {
            a[i] = b[i - 1] * c[i + 1] + b[i] * c[i] + b[i + 1] * c[i - 1];
        }
    }
}

/********************* routine 5 ***********************/

// in the following, size can have any positive value
void Q1_routine_5(float* restrict a, float* restrict b, float* restrict c,
                  int size) {
    for (int i = 0; i < size; i++) {
        if (a[i] > c[i]) {
            float temp = a[i];
            a[i] = c[i];
            c[i] = temp;
        }
        if (a[i] > b[i]) {
            float temp = a[i];
            a[i] = b[i];
            b[i] = temp;
        }
        if (b[i] > c[i]) {
            float temp = b[i];
            b[i] = c[i];
            c[i] = temp;
        }
    }
}

// in the following, size can have any positive value
void Q1_vectorized_5(float* restrict a, float* restrict b, float* restrict c,
                     int size) {
	// load vectors for arrays a, b, and c
	__m128 av, bv, cv, minv, maxv;
	int vsize = size - 3, i;
    for (i = 0; i < vsize; i+=4) {
		av = _mm_loadu_ps(&(a[i]));
		cv = _mm_loadu_ps(&(c[i]));
		minv = _mm_min_ps(av, cv);
		maxv = _mm_max_ps(av, cv);
		_mm_storeu_ps(&(a[i]), minv);
		_mm_storeu_ps(&(c[i]), maxv);

		av = _mm_loadu_ps(&(a[i]));
		bv = _mm_loadu_ps(&(b[i]));
		minv = _mm_min_ps(av, bv);
		maxv = _mm_max_ps(av, bv);
		_mm_storeu_ps(&(a[i]), minv);
		_mm_storeu_ps(&(b[i]), maxv);

		bv = _mm_loadu_ps(&(b[i]));
		cv = _mm_loadu_ps(&(c[i]));
		minv = _mm_min_ps(bv, cv);
		maxv = _mm_max_ps(bv, cv);
		_mm_storeu_ps(&(b[i]), minv);
		_mm_storeu_ps(&(c[i]), maxv);
    }

    for ( ; i < size; i++) {
        if (a[i] > c[i]) {
            float temp = a[i];
            a[i] = c[i];
            c[i] = temp;
        }
        if (a[i] > b[i]) {
            float temp = a[i];
            a[i] = b[i];
            b[i] = temp;
        }
        if (b[i] > c[i]) {
            float temp = b[i];
            b[i] = c[i];
            c[i] = temp;
        }
    }
}

/********************* routine 6 ***********************/

// in the following size is a positive value that is a multiple of 3
float Q1_routine_6(float* restrict a, int size) {
    float x = 0.0;
    float y = 0.0;
    float z = 0.0;
    for (int i = 0; i < size; i = i + 3) {
        x = x + a[i];
        y = y + a[i + 1];
        z = z + a[i + 2];
    }
    return x * y * z;
}

// in the following size is a positive value that is a multiple of 3
float Q1_vectorized_6(float* restrict a, int size) {
    float x = 0.0;
    float y = 0.0;
    float z = 0.0;
	__m128 xv = _mm_loadu_ps(&(a[0]));
	int i, vsize = size - 3;
    for (i = 3; i < size; i += 3) {
		xv = _mm_add_ps(xv, _mm_loadu_ps(&(a[i])));
    }
	float* tmp = aligned_alloc(sizeof(float)*4, 32);
	_mm_store_ps(tmp, xv);
	x = tmp[0]; y = tmp[1]; z = tmp[2];

    
    return x * y * z;
}
