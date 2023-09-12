#include "hardware/regs/addressmap.h"
#include "hardware/regs/m0plus.h"

.syntax unified                 @ Specify unified assembly syntax
.cpu    cortex-m0plus           @ Specify CPU type is Cortex M0+
.thumb                          @ Specify thumb assembly for RP2040
.global main_asm                @ Provide program starting address to the linker
.align 4                        @ Specify code alignment

.equ    SLEEP_TIME, 500         @ Specify the sleep time (in ms)
.equ    LED_GPIO_PIN, 25        @ Specify the pin that the LED is connected to
.equ    LED_GPIO_OUT, 1         @ Specify the direction of the GPIO pin
.equ    LED_VALUE_ON, 1         @ Specify the value that turns the LED "on"
.equ    LED_VALUE_OFF, 0        @ Specify the value that turns the LED "off"
.equ    SVC_ISR_OFFSET, 0x2C    @ The SVC is entry 11 in the vector table
.equ    SVC_MAX_INSTRS, 0x01    @ Maximum allowed SVC subroutines

@ Entry point to the ASM portion of the program
main_asm:
    bl      init_gpio_led       @ Initialise the GPIO LED pin
    bl      install_svc_isr     @ Install the SVC interrupt service routine
loop:
    svc     #0                  @ Call the SVC ISR with value 0 (turns on LED)
    nop                         @ Add a no-op instruction for alignment after SVC
    bl      do_sleep            @ Short pause before proceeding
    svc     #1                  @ Call the SVC ISR with value 1 (turns off LED)
    nop                         @ Add a no-op instruction for alignment after SVC
    bl      do_sleep            @ Add a short pause before proceeding
    b       loop                @ Always jump back to the start of the loop

@ Subroutine used to introduce a short delay in the application
do_sleep:
    @ TODO - add assembly code to implement the sleep delay using sleep_ms
    push    {lr}
    ldr     r0, =SLEEP_TIME             @ Set the value of SLEEP_TIME we want to wait for
    bl      sleep_ms                    @ Sleep until SLEEP_TIME has elapsed then toggle the LED GPIO pin
    pop     {pc}

@ Subroutine used to initialise the PI Pico built-in LED
init_gpio_led:
    @ TODO – add assembly code to initialise the LED GPIO pin
    push    {lr}                        @ Store the link register
    movs    r0, #LED_GPIO_PIN           @ This value is the GPIO LED pin on the PI PICO board
    bl      asm_gpio_init               @ Call the subroutine to initialise the GPIO pin specified by r0
    movs    r0, #LED_GPIO_PIN           @ This value is the GPIO LED pin on the PI PICO board
    movs    r1, #LED_GPIO_OUT           @ We want this GPIO pin to be setup as an output pin
    bl      asm_gpio_set_dir            @ Call the subroutine to set the GPIO pin specified by r0 to state specified r1
    pop     {pc}                        @ Pop the link register to the program counter

@ Subroutine used to install the SVC interrupt service handler
install_svc_isr:
    ldr     r2, =(PPB_BASE + M0PLUS_VTOR_OFFSET)    @ load in the ALARM timer into r2
    ldr     r1, [r2]                                @ load r2 into r1
    movs    r2, #SVC_ISR_OFFSET                     @ load in the ISR offset
    add     r2, r1                                  @ offset the ALARM timer by the offset
    ldr     r0, =svc_isr                            @ Get valid SVC number
    str     r0, [r2]                                @ Store SVC number
    bx      lr                                      @ Return from subroutine

@ SVC interrupt service handler routine
.thumb_func                     @ Required for all interrupt service routines
svc_isr:
    push    {lr}                @ Store the link register
    ldr     r0, [sp, #0x1C]     @ Load offset stack pointer into R0
    subs    r0, #0x2            @ Subtract 2 from SP
    ldr     r0, [r0]            @ Load address at R0
    ldr     r1, =#0xFF          @ Load 256 into r1
    ands    r0, r1              @ AND r0 and r1
    cmp     r0, #SVC_MAX_INSTRS @ if (r0 > max_instructions):
    bgt     svc_done            @   jump to end subroutine
    adr     r1, svc_jmptbl      @ Generate an address
    lsls    r0, #2              @ Logical left shift r0 by 2
    ldr     r1, [r1, r0]        @ Load r1 into the address at r1 offset by r0
    mov     pc, r1              @ Move r1 into the program counter
svc_done:
    pop     {pc}                @ Pop the link register to the program counter

@ First function of SVC subroutine - turn on the LED
svc_num0:
    @ TODO - add assembly code to turn on the LED
    movs    r1, #LED_VALUE_ON           @ The LED is currently "off" so we want to turn it "on"
    movs    r0, #LED_GPIO_PIN           @ Set the LED GPIO pin number to r0 for use by asm_gpio_put
    bl      asm_gpio_put                @ Update the the value of the LED GPIO pin (based on value in r1)
    b       svc_done                    @ Branch back to the main ISR when done

@ Second function of SVC subroutine - turn off the LED
svc_num1:
    @ TODO - add assembly code to turn off the LED
    movs    r1, #LED_VALUE_OFF          @ The LED is currently "on" so we want to turn it "off"
    movs    r0, #LED_GPIO_PIN           @ Set the LED GPIO pin number to r0 for use by asm_gpio_put
    bl      asm_gpio_put                @ Update the the value of the LED GPIO pin (based on value in r1)
    b       svc_done                    @ Branch back to the main ISR when done

@ SVC function entry jump table.
.align 2
svc_jmptbl:
    .word svc_num0              @ Entry zero goes to SVC function #0.
    .word svc_num1              @ Entry one goes to SVC function #1.
    .word 0                     @ Null termination of the jump table.

@ Set data alignment
.data
    .align 4