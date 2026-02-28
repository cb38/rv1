.section .text
.globl main

main:
    addi sp, sp, -16
    sw s0, 12(sp)
    sw s1, 8(sp)

    li a0, 1
    .option push
    .option rvc
    c.addi a0, 5
    c.addi a0, -2
    .option pop
    li t0, 4
    bne a0, t0, fail1

    .option push
    .option rvc
    c.li a1, 7
    .option pop
    li t0, 7
    bne a1, t0, fail2

    .option push
    .option rvc
    c.lui a2, 1
    .option pop
    li t0, 0x1000
    bne a2, t0, fail3

    li a3, 1
    li a4, 3
    .option push
    .option rvc
    c.mv a3, a4
    c.add a3, a4
    .option pop
    li t0, 6
    bne a3, t0, fail4

    li a5, 1
    .option push
    .option rvc
    c.slli a5, 3
    .option pop
    li t0, 8
    bne a5, t0, fail5

    addi s0, sp, 0
    lui s1, 0x5a5aa
    addi s1, s1, 0x5a5
    .option push
    .option rvc
    c.sw s1, 0(s0)
    c.lw a0, 0(s0)
    .option pop
    bne a0, s1, fail6

pass:
    li a0, 0
    lw s1, 8(sp)
    lw s0, 12(sp)
    addi sp, sp, 16
    ret

fail1:
    li a0, 1
    j done
fail2:
    li a0, 2
    j done
fail3:
    li a0, 3
    j done
fail4:
    li a0, 4
    j done
fail5:
    li a0, 5
    j done
fail6:
    li a0, 6

done:
    lw s1, 8(sp)
    lw s0, 12(sp)
    addi sp, sp, 16
    ret
