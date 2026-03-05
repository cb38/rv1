.section .text.startup
.global _start

_start:
    .option push
    .option norvc
    la gp, __global_pointer$
    la sp, __stack_top
    jal ra, main
    j _exit
    .option pop

.global _exit
_exit:
    .option push
    .option norvc
    li t0, 0xc0000008
    sw a0, 0(t0)
1:
    j 1b
    .option pop
