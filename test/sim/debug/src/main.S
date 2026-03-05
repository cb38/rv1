// Debug test program (pure assembly)
// Sets known register values, then spins waiting for a3 to be modified.
// The debug testbench will halt the core, read/write registers, then resume.

.section .text
.globl main

main:
    // Set known values in GPRs
    li a0, 0x12345678       // x10
    lui a1, 0xDEADB         // x11 = 0xDEADB000
    addi a1, a1, -273       // x11 = 0xDEADBEEF (0xDEADB000 + 0xFFFFFEEF = due to sign ext, -273 = 0xFEF, but need to compute)
    // Actually: 0xDEADBEEF = 0xDEADC000 - 0x111 ... let me use proper li sequence
    // The assembler will handle `li` properly for large immediates
    // But let me use explicit lui+addi for clarity
    // li pseudo-instruction will generate lui + addi automatically

    // Reset and use li properly:
    li a0, 0x12345678       // x10
    li a1, 0xDEADBEEF       // x11 (assembler generates lui+addi)
    li a2, 0xCAFEBABE       // x12
    li a3, 0                // x13 - debugger will write 0x42424242 here

    li t0, 0x42424242       // target value

spin:
    beq a3, t0, pass        // debugger wrote a3? -> done
    j spin

pass:
    li a0, 0                // return 0 = PASS
    ret
