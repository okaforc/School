#include "hardware/regs/addressmap.h"
#include "hardware/regs/io_bank0.h"
#include "hardware/regs/timer.h"
#include "hardware/regs/m0plus.h"
#include "hardware/regs/adc.h"

.syntax unified                 @ Specify unified assembly syntax
.cpu    cortex-m0plus           @ Specify CPU type is Cortex M0+
.thumb                          @ Specify thumb assembly for RP2040
.global main_asm                @ Provide program starting address to the linker
.align 4                        @ Specify code alignment

.equ    ALRM_ISR_OFFSET, 0x40           @ ALARM0 is int #0 (vector table entry 16)
.equ    PRINT_DELAY, 1000000           @ ALARM0 is int #0 (vector table entry 16)
.equ    INIT_ADC, 0x7
.equ    READY, 0x80

@ Entry point to the ASM portion of the program
main_asm:

    ldr     r0, =ADC_BASE
    ldr     r1, =INIT_ADC
    ldr     r2, [r0]
    orrs     r2, r1
    str     r2, [r0]

loop:
    bl      set_alarm
    wfi
    ldr     r0, =ADC_BASE
    ldr     r1, =READY
    ldr     r2, [r0]
    ands    r2, r1
    cmp     r2, r1
    bne     loop

    ldr     r1, =INIT_ADC
    ldr     r2, [r0]
    orrs     r2, r1
    str     r2, [r0]

    movs    r1, 10
    ldr     r0, =temperature
    bl      printf


    b       main_asm            @ Infinite loop


//
// Enable alarm timer interrupts and set an alarm
//
set_alarm:
    // Enable alarm timer interrupts using the (TIMER_BASE + TIMER_INTE_OFFSET) register
    // Get the current timer count from (TIMER_BASE + TIMER_TIMELR_OFFSET) register
    // Add the time delay you want to wait for to the current timer count
    // Push the updated value to (TIMER_BASE + TIMER_ALARM0_OFFSET)
    // Exit subroutine

    ldr     r2, =(TIMER_BASE + TIMER_INTE_OFFSET)       @ Get the register at the timer at the INTE offset
    ldr     r3, =1                                      @ Get the value to enable timer interrupts
    str     r3, [r2]                                    @ Enable timer interrupts

    ldr     r2, =(TIMER_BASE + TIMER_TIMELR_OFFSET)     @ Get the register at the timer at the TIMELR offset
    ldr     r1, [r2]                                    @ 
    ldr     r3, =ltimer                                 @ Get the value in ltimer
    ldr     r3, [r3]                                    @ 
    adds    r1, r3                                      @ Add the time delay in ltimer to the timer
    ldr     r2, =(TIMER_BASE + TIMER_ALARM0_OFFSET)     @ 
    str     r1, [r2]                                    @ Push the updated value back

    bx      lr                                          @ Exit subroutine

//
// Sets up the alrm_isr in the RAM vector table and enables the correct interrupt
//
install_alrm_isr:
    // Get the address of the RAM vector table using the (PPB_BASE + M0PLUS_VTOR_OFFSET) register
    // Store the address of the alrm_isr handler to the correct offset for ALARM0 in the vector table
    // Disable the ALARM0 IRQ by writing the correct value to (PPB_BASE + M0PLUS_NVIC_ICPR_OFFSET)
    // Enable the ALARM0 IRQ by writing the correct value to (PPB_BASE + M0PLUS_NVIC_ISER_OFFSET)
    // Exit subroutine
    ldr     r2, =(PPB_BASE + M0PLUS_VTOR_OFFSET)        @ load in the ALARM timer into r2
    ldr     r1, [r2]                                    @ load r2 into r1 
    movs    r2, #ALRM_ISR_OFFSET                        @ load in the ISR offset
    add     r2, r1                                      @ offset the ALARM timer by the offset
    ldr     r0, =alrm_isr                               @ Get the address of the alrm_isr handler
    str     r0, [r2]                                    @ Store SVC number

    movs    r3, #1                                      @ Load in the value to disable and anable the alarm
    ldr     r2, =(PPB_BASE + M0PLUS_NVIC_ICPR_OFFSET)   @ Get the alarm offset by the ICPR offset
    str     r3, [r2]                                    @ Disable the ALARM0 IRQ
    ldr     r2, =(PPB_BASE + M0PLUS_NVIC_ISER_OFFSET)   @ Get the alarm offset by the ISER offset
    str     r3, [r2]                                    @ Enable the ALARM0 IRQ

    bx      lr                                          @ Return from subroutine

//
// Service the pending interrupt from the ALARM0 TIMER
//
.thumb_func
alrm_isr:
    // Perform required functionality (e.g. toggle the LED)
    // Disable pending interrupt from TIMER by writing correct value to (TIMER_BASE + TIMER_INTR_OFFSET)
    // Exit ISR
    push {lr}

    ldr r2, =(TIMER_BASE + TIMER_INTR_OFFSET)       @ Get timer register
    movs r1, #1                                     @ 
    str r1, [r2]                                    @ Disable alarm interrupt

    
end_alarm:
    pop {pc}


@ Set data alignment
.data
    .align 4
    temperature:
        .asciz "Temperature: %.1f degrees\n"
.data
ltimer: .word   PRINT_DELAY