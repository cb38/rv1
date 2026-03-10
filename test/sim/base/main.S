.section .text.startup
.global _start
_start:
    
    lui x1, 255
    lui x2, 32
    add x3, x1, x2
    add x4, x1, x2
    addi x2, x0, 160
    sw x4, 4(x2)
    sw x4, 0(x2)
    lw x5, 4(x2)
    lw x6, 4(x2)
    lhu x3, 2(x2)
    lbu x7, 1(x2)
    lui x1, 1
    jal x4, end
    lui x2, 2
    lui x3, 3
    lui x4, 4
end:
    lui x5, 5
    lui x6, 6
    sub x7, x5, x6
    add x1, x7, x1
    
    # Exit with code 0
    li x10, 0
    li x28, 0xc0000008
    sw x10, 0(x28)
1:
    j 1b