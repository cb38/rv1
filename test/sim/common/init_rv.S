.section .text.startup
.global _start
_start:
    la gp, __global_pointer$
    la sp, __stack_top
    call main
    j _exit

.global _exit
_exit:
    li t0, 0xc0000008
    sw a0, 0(t0)
1:
    j 1b
