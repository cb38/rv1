# ─────────────────────────────────────────────────────────────────────────────
# Atomics (A extension) smoke test — cxxrtl / tb_rv version
#
# Called from init_rv.S as: main()  (RISC-V C ABI)
# Returns:  a0 == 0  → PASS,  a0 != 0 → FAIL (bit N = sub-test N+1 failed)
#
# Uses: a0 (error acc), a1 (scratch base), a2-a6, t0-t1  (all caller-saved)
# Scratch area: 0x80001000 (inside the 16 KiB SRAM, above .text region)
# ─────────────────────────────────────────────────────────────────────────────

.section .text
.global main
main:
    li   a0, 0                  # error accumulator (0 = all pass)
    lui  a1, 0x80001            # a1 = 0x80001000  (scratch word in SRAM)

    # =====================================================================
    # Test 1: LR.W / SC.W  — basic successful sequence
    # =====================================================================
    li   a2, 0x42
    sw   a2, 0(a1)              # mem[a1] = 0x42

    lr.w a3, (a1)               # a3 = load-reserved  → expect 0x42
    li   a4, 0x99
    sc.w a5, a4, (a1)           # store-conditional → mem[a1];  a5 = 0 on success

    lw   a6, 0(a1)              # re-read mem
    li   t0, 0x99
    bne  a3, a2, .fail1         # loaded value must be 0x42
    bne  a5, x0, .fail1         # SC must succeed (rd = 0)
    bne  a6, t0, .fail1         # stored value must be 0x99
    j    .test2
.fail1:
    ori  a0, a0, 1

    # =====================================================================
    # Test 2: SC.W fails when no prior LR
    # =====================================================================
.test2:
    li   a2, 0xAA
    sw   a2, 0(a1)              # mem[a1] = 0xAA
    li   a4, 0xBB
    sc.w a5, a4, (a1)           # should FAIL → a5 != 0

    lw   a6, 0(a1)
    beq  a5, x0, .fail2         # SC must fail (rd != 0)
    bne  a6, a2,  .fail2        # memory must be unchanged
    j    .test3
.fail2:
    ori  a0, a0, 2

    # =====================================================================
    # Test 3: AMOSWAP.W
    # =====================================================================
.test3:
    li   a2, 0x10
    sw   a2, 0(a1)
    li   a4, 0x20
    amoswap.w a5, a4, (a1)     # a5 = old (0x10),  mem = 0x20

    lw   a6, 0(a1)
    li   t0, 0x20
    bne  a5, a2, .fail3
    bne  a6, t0, .fail3
    j    .test4
.fail3:
    ori  a0, a0, 4

    # =====================================================================
    # Test 4: AMOADD.W
    # =====================================================================
.test4:
    li   a2, 100
    sw   a2, 0(a1)
    li   a4, 23
    amoadd.w a5, a4, (a1)      # a5 = old (100),  mem = 123

    lw   a6, 0(a1)
    li   t0, 123
    bne  a5, a2, .fail4
    bne  a6, t0, .fail4
    j    .test5
.fail4:
    ori  a0, a0, 8

    # =====================================================================
    # Test 5: AMOXOR.W
    # =====================================================================
.test5:
    li   a2, 0xFF
    sw   a2, 0(a1)
    li   a4, 0x0F
    amoxor.w a5, a4, (a1)      # a5 = old (0xFF),  mem = 0xF0

    lw   a6, 0(a1)
    li   t0, 0xF0
    bne  a5, a2, .fail5
    bne  a6, t0, .fail5
    j    .test6
.fail5:
    ori  a0, a0, 16

    # =====================================================================
    # Test 6: AMOAND.W
    # =====================================================================
.test6:
    li   a2, 0xFF
    sw   a2, 0(a1)
    li   a4, 0x0F
    amoand.w a5, a4, (a1)      # a5 = old (0xFF),  mem = 0x0F

    lw   a6, 0(a1)
    li   t0, 0x0F
    bne  a5, a2, .fail6
    bne  a6, t0, .fail6
    j    .test7
.fail6:
    ori  a0, a0, 32

    # =====================================================================
    # Test 7: AMOOR.W
    # =====================================================================
.test7:
    li   a2, 0xF0
    sw   a2, 0(a1)
    li   a4, 0x0F
    amoor.w a5, a4, (a1)       # a5 = old (0xF0),  mem = 0xFF

    lw   a6, 0(a1)
    li   t0, 0xFF
    bne  a5, a2, .fail7
    bne  a6, t0, .fail7
    j    .test8
.fail7:
    ori  a0, a0, 64

    # =====================================================================
    # Test 8: AMOMIN.W  (signed)
    # =====================================================================
.test8:
    li   a2, 5
    sw   a2, 0(a1)
    li   a4, -3
    amomin.w a5, a4, (a1)      # min(5,-3)=-3;  a5=old(5), mem=-3

    lw   a6, 0(a1)
    li   t0, -3
    bne  a5, a2, .fail8
    bne  a6, t0, .fail8
    j    .test9
.fail8:
    ori  a0, a0, 128

    # =====================================================================
    # Test 9: AMOMAX.W  (signed)
    # =====================================================================
.test9:
    li   a2, -10
    sw   a2, 0(a1)
    li   a4, 7
    amomax.w a5, a4, (a1)      # max(-10,7)=7;  a5=old(-10), mem=7

    lw   a6, 0(a1)
    li   t0, 7
    bne  a5, a2, .fail9
    bne  a6, t0, .fail9
    j    .test10
.fail9:
    ori  a0, a0, 256

    # =====================================================================
    # Test 10: AMOMINU.W  (unsigned)
    # =====================================================================
.test10:
    li   a2, 3
    sw   a2, 0(a1)
    li   a4, -1                 # 0xFFFFFFFF (huge unsigned)
    amominu.w a5, a4, (a1)     # minu(3, big)=3;  a5=old(3), mem=3

    lw   a6, 0(a1)
    bne  a5, a2, .fail10
    bne  a6, a2, .fail10
    j    .test11
.fail10:
    ori  a0, a0, 512

    # =====================================================================
    # Test 11: AMOMAXU.W  (unsigned)
    # =====================================================================
.test11:
    li   a2, 3
    sw   a2, 0(a1)
    li   a4, -1                 # 0xFFFFFFFF (huge unsigned)
    amomaxu.w a5, a4, (a1)     # maxu(3, 0xFFFFFFFF)=0xFFFFFFFF;  a5=old(3), mem=0xFFFFFFFF

    lw   a6, 0(a1)
    li   t0, -1
    bne  a5, a2,  .fail11
    bne  a6, t0,  .fail11
    j    .done
.fail11:
    ori  a0, a0, 1024

    # =====================================================================
    # Done — a0 == 0 means all tests passed; _exit(a0) signals the testbench
    # =====================================================================
.done:
    ret
