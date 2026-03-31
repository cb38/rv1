// debug_fpga firmware
// Sets known register values then loops forever.
// The sim testbench will halt, verify registers, test mem read/write, resume.

.section .text.startup
.global _start
_start:
    // establish stack + GP (same as other tests start.S)
    .option push
    .option norvc
    la gp, __global_pointer$
    la sp, __stack_top
    .option pop

    // Load known sentinel values into registers
    li  x10, 0x12345678   // a0
    li  x11, 0xDEAD0000   // a1 (upper half – avoids sign-extend complexity)
    li  x12, 0x0000BEEF   // a2
    li  x13, 0x00000000   // a3 – will be written by debugger

    // Increment x13 each iteration so the waveform shows the loop running
loop:
    addi x13, x13, 1
    j    loop
