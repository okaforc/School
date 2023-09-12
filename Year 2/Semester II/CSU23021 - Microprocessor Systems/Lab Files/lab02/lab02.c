//#define WOKWI             // Uncomment if running on Wokwi RP2040 emulator.

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "float.h"
// #include "pico/stdlib.h"

double PI_DEFAULT = 3.14159265359; // default value for pi, to use in a comparison
int MAX_ITER = 100000; // maximum number of iterations used for Wallis approx.

float wallis_f();
double wallis_d();

/**
 * @brief EXAMPLE - HELLO_C
 *        Simple example to initialise the IOs and then
 *        print a "Hello World!" message to the console.
 *
 * @return int  Application return code (zero for success).
 */
int main() {

#ifndef WOKWI
    // Initialise the IO as we will be using the UART
    // Only required for hardware and not needed for Wokwi
    // stdio_init_all();
#endif

    // Print a console message to inform user what's going on.
    printf("Value for comparison (PI): %.11f\n\n", PI_DEFAULT);
    printf("Single-Precision (Float) Floating-Point: %.11f\nApproximation Error: %.11f\n\n", wallis_f(), PI_DEFAULT-wallis_f());
    printf("Double-Precision (Double) FLoating-Point : %.11f\nApproximation Error: %.11f\n", wallis_d(), PI_DEFAULT - wallis_d());

    // Returning zero indicates everything went okay.
    return 0;
}

/**
 * @brief wallis_f
 *        Calculates the Wallis approximation for pi with single-precision (float) floating-point representation
 *
 * @return The float approximation of pi
 */
float wallis_f() {
    float pi_temp = 1;
    for(int i = 1; i < MAX_ITER; i++) {
        pi_temp *= ((2.0 * i) / ((2.0 * i) - 1.0)) * ((2.0 * i) / ((2.0 * i) + 1.0));  /* repeatedly multiply result with increasing i */
    }
    
    return pi_temp*2; /* Wallis approximation returns pi/2, so multiply by 2 to get pi approximation */
}

/**
 * @brief wallis_d
 *        Calculates the Wallis approximation for pi with double-precision (double) floating-point representation
 *
 * @return The double approximation of pi
 */
double wallis_d() {
    double pi_temp = 1;
    for (int i = 1; i < MAX_ITER; i++) {
        pi_temp *= ((2.0 * i) / ((2.0 * i) - 1.0)) * ((2.0 * i) / ((2.0 * i) + 1.0));
    }

    return pi_temp * 2;
}