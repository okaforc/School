#include "pico/stdlib.h"

/**
 * @brief EXAMPLE - BLINK_C
 *        Simple example to initialise the built-in LED on
 *        the Raspberry Pi Pico and then flash it forever.
 *
 * @return int  Application return code (zero for success).
 */

 /**
  * @brief loopLight
  *        Performs an infinite loop that toggles on and off a given
  *        LED pin with a given delay in milliseconds.
  *
  *  @param LED_PIN The ID of the LED pin to be toggled.
  *  @param LED_DELAY The time the loop shoudl delay for in milliseconds.
  *  @return Void.
  */
void loopLight(uint LED_PIN, uint LED_DELAY) {
    // Do forever...
    while (true) {

        // Toggle the LED on and then sleep for delay period
        gpio_put(LED_PIN, 1);
        sleep_ms(LED_DELAY);

        // Toggle the LED off and then sleep for delay period
        gpio_put(LED_PIN, 0);
        sleep_ms(LED_DELAY);

    }
}


int main() {

    // Specify the PIN number and sleep delay
    const uint LED_PIN = 25;
    const uint LED_DELAY = 500;

    // Setup the LED pin as an output.
    gpio_init(LED_PIN);
    gpio_set_dir(LED_PIN, GPIO_OUT);

    loopLight(LED_PIN, LED_DELAY);

    // Should never get here due to infinite while-loop.
    return 0;

}