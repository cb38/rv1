/* Minimal crt0 for the fpga firmware test.
 * No testbench exit mechanism — the firmware loops forever (same as on real HW). */
.section .text.init, "ax"
.global _start
_start:
    la    sp, __stack_top
    call  main
1:  j     1b          /* hang if main returns */
