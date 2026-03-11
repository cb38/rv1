#define IO_BASE 0xc0000000
#define IO_PRINT_CHAR (IO_BASE + 0x0)
#define IO_PRINT_U32  (IO_BASE + 0x4)
#define IO_EXIT       (IO_BASE + 0x8)

// Use .text.startup section so that _start is placed first in .text by the linker
.section .text.startup
.global _start
_start:
    // Enable mcycle counting (clear mcountinhibit bits 0 and 2)
    la a0, 1f
    csrw mtvec, a0            // catch trap if CSR doesn't exist
    csrci 0x320, 0x5          // mcountinhibit: clear cy(0) and ir(2)
    j 2f
.p2align 2
1:
    csrw mcause, zero         // swallow the trap
2:
    // Set up trap vector for runtime traps
    la a0, _trap_handler
    csrw mtvec, a0

.option push
.option norelax
    la gp, __global_pointer$
.option pop
    la sp, __stack_top

    // newlib _start expects argc, argv on the stack. Leave stack 16-byte aligned.
    addi sp, sp, -16
    la t0, progname
    sw t0, 0(sp)              // argv[0] = progname
    sw zero, 4(sp)            // argv[1] = NULL
    li a0, 1                  // argc = 1
    mv a1, sp                 // argv = sp

    call main
    j _exit

.global _exit
_exit:
    li t0, IO_EXIT
    sw a0, 0(t0)
1:
    j 1b

.global _sbrk
_sbrk:
    la a1, heap_ptr
    lw a2, (a1)
    add a0, a0, a2
    sw a0, (a1)
    mv a0, a2
    ret

.p2align 2
heap_ptr:
    .word _end

progname:
    .asciz "rv-coremark"
.p2align 2

// Simple trap handler: print diagnostics and exit with -1
_trap_handler:
    li t1, IO_BASE
    la t0, _str_trap
    jal tp, _tb_puts
    csrr t0, mepc
    sw t0, IO_PRINT_U32 - IO_BASE(t1)
    la t0, _str_mcause
    jal tp, _tb_puts
    csrr t0, mcause
    sw t0, IO_PRINT_U32 - IO_BASE(t1)
    li t2, -1
    sw t2, IO_EXIT - IO_BASE(t1)
1:
    j 1b

_tb_puts:
1:
    lbu t2, (t0)
    addi t0, t0, 1
    beqz t2, 2f
    sw t2, IO_PRINT_CHAR - IO_BASE(t1)
    j 1b
2:
    jr tp

_str_trap:   .asciz "*** TRAP @ mepc="
_str_mcause: .asciz " mcause="

