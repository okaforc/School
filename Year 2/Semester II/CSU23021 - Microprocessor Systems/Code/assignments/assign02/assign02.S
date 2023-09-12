#include "hardware/regs/addressmap.h"
#include "hardware/regs/io_bank0.h"
#include "hardware/regs/timer.h"
#include "hardware/regs/m0plus.h"

.syntax unified                                         @ Specify unified assembly syntax
.cpu    cortex-m0plus                                   @ Specify CPU type is Cortex M0+
.thumb                                                  @ Specify thumb assembly for RP2040
.global main_asm                                        @ Provide program starting address to the linker
.extern printf                                          @ include external printf function
.align 4                                                @ Specify code alignment

.equ    GPIO_BTN_EN,  21                                @ Specify pin for the button in use              @ Specify input direction for a GPIO pin
.equ    DFLT_ALARM_TIME, 1000000                        @ Specify the default alarm timeout

.equ    GPIO_BTN_RAIS_MSK, 0x00800000                   @ Bit-23 for rising-edge event on GP21
.equ    GPIO_BTN_FALL_MSK, 0x00400000                   @ Bit-22 for falling-edge event on GP21

.equ    GPIO_ISR_OFFSET, 0x74                           @ GPIO is int #13 (vector table entry 29)
.equ    ALRM_ISR_OFFSET, 0x40                           @ ALARM0 is int #0 (vector table entry 16)

main_asm:
    push    {lr}
    movs    r7, #0
    bl      init_btns                                   @ branch to initialize GP21 as input sourece, and enable msg_raising and falling edge interruption
    bl      install_gpio_isr                            @ branch to subroutine that installs the gpio isr
    bl      install_alrm_isr                            @ branch to subroutine that installs the alarm isr
main_loop:
    cmp     r7, #2                                      @ if r7 is 1, exit the loop and asm_main will exist
    beq     endMain
    cmp     r7, #1
    bne     skipAddSpace
    bl      set_alarm
    movs    r0, #0
    bl      asm_append_delay
skipAddSpace:
    wfi                                                 @ wait for an interrupt
    b       main_loop                                   @ infinite loop
endMain:
    pop     {pc}

init_btns:
    push    {lr}
    movs    r0, #GPIO_BTN_EN                            @ Use asm_gpio_set_irq C function to setup msg_raising and falling-edge interrutps
    bl      asm_gpio_set_irq
    pop     {pc}

@ Sets up the gpio_isr in the RAM vector table and enables the correct interrupt
install_gpio_isr:
    ldr     r2, =(PPB_BASE + M0PLUS_VTOR_OFFSET)        @ Get the address of the RAM vector table using the (PPB_BASE + M0PLUS_VTOR_OFFSET) register
    ldr     r1, [r2]                                    @ store contands in the address of the RAM vactor table into r1 for later use
    movs    r2, #GPIO_ISR_OFFSET
    add     r2, r1                                      @ get the correct offset to install the gpio interruption handler
    ldr     r0, =gpio_isr                               @ get the value of the gpio interruption handler exception            
    str     r0, [r2]                                    @ store the address of the gpio_isr handler to the correct offset for GPIO in the vector table
    ldr     r1, =(PPB_BASE + M0PLUS_NVIC_ICPR_OFFSET)
    movs    r0, #1                                      @ from the interrup source
    lsls    r0, #13                                
    str     r0 ,[r1]                                    @ Disable the GPIO IRQ by writing the correct value to (PPB_BASE + M0PLUS_NVIC_ICPR_OFFSET)
    ldr     r1, =(PPB_BASE + M0PLUS_NVIC_ISER_OFFSET)                              
    str     r0 ,[r1]                                    @ Enable the GPIO IRQ by writing the correct value to (PPB_BASE + M0PLUS_NVIC_ISER_OFFSET)
    bx      lr                                          @ Exit subroutine

@ Service the pending interrupt from the GPIO
.thumb_func
gpio_isr:
    push    {lr}
    movs    r7, #0
    @ Read the interrupt status event from the (IO_BANK0_BASE + IO_BANK0_PROC0_INTS2_OFFSET) register
    ldr     r0, =(IO_BANK0_BASE + IO_BANK0_PROC0_INTS2_OFFSET)  @ get address of bank register
    ldr     r1, [r0]                                            @ load register from address

    ldr     r2, =GPIO_BTN_RAIS_MSK                              @ store mask in register 
    cmp     r1, r2                                              @ detect current is at falling or msg_raising edge
    beq     rising_edge                                         @ if the button is released...
    ldr     r0, =msg_falling
    bl      asm_mark_time
    mov     r5, r0                                              @ store the time when GP21 is pushed down to r5
    ldr     r0, =GPIO_BTN_FALL_MSK                              @ load falling-edge mask to clearing interrupt
    b       finish

rising_edge:
    ldr     r0, =msg_raising
    bl      set_alarm                                           @ set up and alarm to end the program if no new gpio exception is handled after 2 seconds when the button is rleased
    bl      asm_mark_time                                       @ get the current time
    subs    r5, r0, r5                                          @ get time of holding the button (time-stamp @ msg_raising-edge - time-stamp @ falling-edge)
    
    // ! TODO: pass the holding time stored in r5 to a c function in order to update the dash-dot sequence
    movs    r0, r5
    bl      asm_append_delay

    ldr     r0, =GPIO_BTN_RAIS_MSK                              @ load rising edge mask for clearing interrupt
finish:   
    ldr     r1, =(IO_BANK0_BASE + IO_BANK0_INTR2_OFFSET)        @ get address of bank INTR2 register    
    str     r0, [r1]                                            @ write stored mask to the bank INTR2 register
    pop     {pc}                                                @ Exit ISR


install_alrm_isr:   
    ldr     r2, =(PPB_BASE + M0PLUS_VTOR_OFFSET)                @ Get the address of the RAM vector table using the (PPB_BASE + M0PLUS_VTOR_OFFSET) register
    ldr     r1, [r2]    
    movs    r2, #ALRM_ISR_OFFSET    
    add     r2, r1  
    ldr     r0, =alrm_isr   
    str     r0, [r2]                                            @ Store the address of the alrm_isr handler to the correct offset for ALARM0 in the vector table
    movs    r0, #1
    ldr     r2, =(PPB_BASE + M0PLUS_NVIC_ICPR_OFFSET)
    str     r0, [r2]                                            @ Disable the ALARM0 IRQ by writing the correct value to (PPB_BASE + M0PLUS_NVIC_ICPR_OFFSET)
    ldr     r2, =(PPB_BASE + M0PLUS_NVIC_ISER_OFFSET)
    str     r0, [r2]                                            @ Enable the ALARM0 IRQ by writing the correct value to (PPB_BASE + M0PLUS_NVIC_ISER_OFFSET)
    bx      lr                                                  @ Exit subroutine       


.thumb_func
alrm_isr:
    push    {lr}                                       @ Disable pending interrupt from TIMER by writing correct value to (TIMER_BASE + TIMER_INTR_OFFSET)
    movs    r0, #1
    ldr     r1, =(TIMER_BASE + TIMER_INTR_OFFSET)      
    str     r0, [r1]                                   
    adds    r7, #1                                     @ update r7 to true to exist the program
    pop     {pc}                                      


set_alarm:
    movs    r0, #1
    ldr     r2, =(TIMER_BASE + TIMER_INTE_OFFSET)       @ Enable alarm timer interrupts using the (TIMER_BASE + TIMER_INTE_OFFSET) register
    str     r0, [r2]                                    
    ldr     r2, =(TIMER_BASE + TIMER_TIMELR_OFFSET) 
    ldr     r0, [r2]                                    @ Get the current timer count from (TIMER_BASE + TIMER_TIMELR_OFFSET) register
    ldr     r2, =DFLT_ALARM_TIME
    add     r0, r2                                      @ Add the time delay you want to wait for to the current timer count
    ldr     r2, =(TIMER_BASE + TIMER_ALARM0_OFFSET) 
    str     r0, [r2]                                    @ Push the updated value to (TIMER_BASE + TIMER_ALARM0_OFFSET)
    bx      lr                                          @ Exit subroutine


.align 4
    msg_raising:      .asciz "raising \n"
    msg_falling:      .asciz "falling \n"
.data
    current_time:   .word   0
    time_diff:      .word   0
    morse_seq:      .word   0
