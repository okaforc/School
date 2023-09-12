//#define WOKWI             // Uncomment if running on Wokwi RP2040 emulator.

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#include "pico/multicore.h" // Required for using multiple cores on the RP2040.
#include "pico/stdlib.h"
#include "pico/float.h"     // Required for using single-precision variables.
#include "pico/double.h"    // Required for using double-precision variables.

double PI_DEFAULT = 3.14159265359; // default value for pi, to use in a comparison
int MAX_ITER = 100000; // maximum number of iterations used for Wallis approx.
double MICRO_TO_NORM = 1000000.0; // 10^6, to change microseconds into normal seconds

// unsigned int XIP_CTRL_BASE = 0x14000000;


uint64_t full_st, full_et, d_wallis_time, f_wallis_time;
double full_elapsed, single_elapsed, double_elapsed;
float wallis_f();
double wallis_d();
// function to run both wallis functions using a single core and print the results
void run_single_core();
// function to run both wallis functions using both cores and print the results
void run_dual_core();

// Function to get the enable status of the XIP cache
bool get_xip_cache_en();

// Function to set the enable status of the XIP cache
bool set_xip_cache_en(bool cache_en);


/**
 * @brief This function acts as the main entry-point for core #1.
 *        A function pointer is passed in via the FIFO with one
 *        incoming int32_t used as a parameter. The function will
 *        provide an int32_t return value by pushing it back on
 *        the FIFO, which also indicates that the result is ready.
 */
void core1_entry() {
    while (1) {
        // 
        int32_t(*func)() = (int32_t(*)()) multicore_fifo_pop_blocking();
        int32_t p = multicore_fifo_pop_blocking();
        int32_t result = (*func)(p);
        multicore_fifo_push_blocking(result);
    }
}


int main() {

    stdio_init_all();
    multicore_launch_core1(core1_entry);


    /* Sqeuential */
    printf("Single Core\n");
    set_xip_cache_en(1); // turn on caching
    printf("Cache enabled: %d\n", get_xip_cache_en());
    run_single_core(); // run single core with cache enabled
    set_xip_cache_en(0); // turn off caching
    printf("Cache disabled: %d\n", get_xip_cache_en());
    run_single_core(); // run single core with cache disabled

    /* Parallel */
    printf("\nDual Core\n");
    set_xip_cache_en(1);
    printf("Cache enabled: %d\n", get_xip_cache_en());
    run_dual_core(); // run dual core with cache enabled
    set_xip_cache_en(0);
    printf("Cache disabled: %d\n", get_xip_cache_en());
    run_dual_core(); // run dual core with cache disabled


    // Returning zero indicates everything went okay.
    return 0;
}

void run_single_core() {
    printf("Sequential System (single-core)\n");

    full_st = time_us_64(); // start a stopwatch
    wallis_f(MAX_ITER); // call the function (return value isn't needed and discarded)
    wallis_d(MAX_ITER);
    full_et = time_us_64(); // stop the stopwatch

    full_elapsed = (full_et - full_st) / MICRO_TO_NORM; // calculate the full elapsed time for the stopwatch
    single_elapsed = f_wallis_time / MICRO_TO_NORM;
    double_elapsed = d_wallis_time / MICRO_TO_NORM;

    printf("Time taken for Single-Precision (Float) Floating-Point: %f seconds\n", single_elapsed);
    printf("Time taken for Double-Precision (Double) Floating-Point: %f seconds\n", double_elapsed);
    printf("Full Time: %f seconds\n\n", full_elapsed);
}


void run_dual_core() {
    printf("Parallel System (dual-core)\n");

    full_st = time_us_64(); // start a stopwatch
    multicore_fifo_push_blocking((uintptr_t)&wallis_d); // push the address of the function
    multicore_fifo_push_blocking(MAX_ITER); // push the paramaters of the function

    wallis_f(MAX_ITER); // call the function (return value isn't needed and discarded)
    single_elapsed = f_wallis_time / MICRO_TO_NORM; // get elapsed time (in microseconds) in seconds
    printf("Time taken for Single-Precision (Float) Floating-Point: %f seconds\n", single_elapsed); 

    multicore_fifo_pop_blocking(); // pop off the result
    double_elapsed = d_wallis_time / MICRO_TO_NORM;
    printf("Time taken for Double-Precision (Double) Floating-Point: %f seconds\n", double_elapsed);
    full_et = time_us_64(); // end the stopwatch
    full_elapsed = (full_et - full_st) / MICRO_TO_NORM; // calculate the time taken

    printf("Full Time: %f seconds\n\n", full_elapsed);
}


/**
 * @brief wallis_f
 *        Calculates the Wallis approximation for pi with single-precision (float) floating-point representation
 *
 * @return The float approximation of pi
 */
float wallis_f(int max_iter) {
    uint64_t single_st = time_us_64();
    float pi_temp = 1;
    for (int i = 1; i < max_iter; i++) {
        pi_temp *= ((2.0 * i) / ((2.0 * i) - 1.0)) * ((2.0 * i) / ((2.0 * i) + 1.0));  /* repeatedly multiply result with increasing i */
    }
    pi_temp *= 2; /* Wallis approximation returns pi/2, so multiply by 2 to get pi approximation */
    uint64_t single_et = time_us_64();

    f_wallis_time = single_et - single_st;
    return pi_temp;
}

/**
 * @brief wallis_d
 *        Calculates the Wallis approximation for pi with double-precision (double) floating-point representation
 *
 * @return The double approximation of pi
 */
double wallis_d(int max_iter) {
    uint64_t double_st = time_us_64();
    double pi_temp = 1;
    for (int i = 1; i < max_iter; i++) {
        pi_temp *= ((2.0 * i) / ((2.0 * i) - 1.0)) * ((2.0 * i) / ((2.0 * i) + 1.0));
    }
    pi_temp *= 2;
    uint64_t double_et = time_us_64();
    d_wallis_time = double_et - double_st;
    return pi_temp;
}

bool get_xip_cache_en() {
    const uint8_t* cache_content = (const uint8_t*)(XIP_CTRL_BASE); // read the memory at the XIP control base
    return cache_content[0]; // return the value set at position 0
}

bool set_xip_cache_en(bool cache_en) {
    uint8_t* cache_content = (uint8_t*)(XIP_CTRL_BASE); // access the memory at the XIP control base
    cache_content[0] = cache_en; // set the value at position 0 to the value in cache_en
    return 0;
}