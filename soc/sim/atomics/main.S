# ─────────────────────────────────────────────────────────────────────────────
# Atomics (A extension) smoke test
#
# Tests LR.W / SC.W and every AMO*.W instruction.
# Result convention:  x1 == 0  → PASS,  x1 != 0 → FAIL (bit = failing test #)
# ─────────────────────────────────────────────────────────────────────────────

.section .text.startup
.global _start
_start:
    li   x1, 0                  # error accumulator (0 = all pass)
    lui  x2, 0x80001            # x2 = 0x80001000  (scratch area in SRAM)

    # =====================================================================
    # Test 1: LR.W / SC.W  — basic successful sequence
    # =====================================================================
    li   x3, 0x42
    sw   x3, 0(x2)              # mem[x2] = 0x42

    lr.w x4, (x2)               # x4 = load-reserved  → expect 0x42
    li   x5, 0x99
    sc.w x6, x5, (x2)           # store-conditional x5 → mem[x2];  x6 = 0 on success

    lw   x7, 0(x2)              # re-read mem
    li   x8, 0x99
    bne  x4, x3, .fail1         # loaded value must be 0x42
    bne  x6, x0, .fail1         # SC must succeed (rd = 0)
    bne  x7, x8, .fail1         # stored value must be 0x99
    j    .test2
.fail1:
    ori  x1, x1, 1              # set bit 0

    # =====================================================================
    # Test 2: SC.W fails when no prior LR
    # =====================================================================
.test2:
    li   x3, 0xAA
    sw   x3, 0(x2)              # mem[x2] = 0xAA
    # (no LR.W here — reservation was consumed by test 1's SC)
    li   x5, 0xBB
    sc.w x6, x5, (x2)           # should FAIL → x6 != 0

    lw   x7, 0(x2)
    li   x8, 0xAA
    beq  x6, x0, .fail2         # SC must fail (rd != 0)
    bne  x7, x8, .fail2         # memory must be unchanged
    j    .test3
.fail2:
    ori  x1, x1, 2              # set bit 1

    # =====================================================================
    # Test 3: AMOSWAP.W
    # =====================================================================
.test3:
    li   x3, 0x10
    sw   x3, 0(x2)
    li   x5, 0x20
    amoswap.w x6, x5, (x2)     # x6 = old (0x10),  mem = 0x20

    lw   x7, 0(x2)
    li   x8, 0x10
    li   x9, 0x20
    bne  x6, x8, .fail3
    bne  x7, x9, .fail3
    j    .test4
.fail3:
    ori  x1, x1, 4              # set bit 2

    # =====================================================================
    # Test 4: AMOADD.W
    # =====================================================================
.test4:
    li   x3, 100
    sw   x3, 0(x2)
    li   x5, 23
    amoadd.w x6, x5, (x2)      # x6 = old (100),  mem = 123

    lw   x7, 0(x2)
    li   x8, 100
    li   x9, 123
    bne  x6, x8, .fail4
    bne  x7, x9, .fail4
    j    .test5
.fail4:
    ori  x1, x1, 8              # set bit 3

    # =====================================================================
    # Test 5: AMOXOR.W
    # =====================================================================
.test5:
    li   x3, 0xFF
    sw   x3, 0(x2)
    li   x5, 0x0F
    amoxor.w x6, x5, (x2)      # x6 = old (0xFF),  mem = 0xF0

    lw   x7, 0(x2)
    li   x8, 0xFF
    li   x9, 0xF0
    bne  x6, x8, .fail5
    bne  x7, x9, .fail5
    j    .test6
.fail5:
    ori  x1, x1, 16             # set bit 4

    # =====================================================================
    # Test 6: AMOAND.W
    # =====================================================================
.test6:
    li   x3, 0xFF
    sw   x3, 0(x2)
    li   x5, 0x0F
    amoand.w x6, x5, (x2)      # x6 = old (0xFF),  mem = 0x0F

    lw   x7, 0(x2)
    li   x8, 0xFF
    li   x9, 0x0F
    bne  x6, x8, .fail6
    bne  x7, x9, .fail6
    j    .test7
.fail6:
    ori  x1, x1, 32             # set bit 5

    # =====================================================================
    # Test 7: AMOOR.W
    # =====================================================================
.test7:
    li   x3, 0xF0
    sw   x3, 0(x2)
    li   x5, 0x0F
    amoor.w x6, x5, (x2)       # x6 = old (0xF0),  mem = 0xFF

    lw   x7, 0(x2)
    li   x8, 0xF0
    li   x9, 0xFF
    bne  x6, x8, .fail7
    bne  x7, x9, .fail7
    j    .test8
.fail7:
    ori  x1, x1, 64             # set bit 6

    # =====================================================================
    # Test 8: AMOMIN.W  (signed)
    # =====================================================================
.test8:
    li   x3, 5
    sw   x3, 0(x2)
    li   x5, -3                 # 0xFFFFFFFD
    amomin.w x6, x5, (x2)      # min(5, -3) = -3;  x6 = old (5), mem = -3

    lw   x7, 0(x2)
    li   x8, 5
    li   x9, -3
    bne  x6, x8, .fail8
    bne  x7, x9, .fail8
    j    .test9
.fail8:
    ori  x1, x1, 128            # set bit 7

    # =====================================================================
    # Test 9: AMOMAX.W  (signed)
    # =====================================================================
.test9:
    li   x3, -10
    sw   x3, 0(x2)
    li   x5, 7
    amomax.w x6, x5, (x2)      # max(-10, 7) = 7;  x6 = old (-10), mem = 7

    lw   x7, 0(x2)
    li   x8, -10
    li   x9, 7
    bne  x6, x8, .fail9
    bne  x7, x9, .fail9
    j    .test10
.fail9:
    ori  x1, x1, 256            # set bit 8

    # =====================================================================
    # Test 10: AMOMINU.W  (unsigned)
    # =====================================================================
.test10:
    li   x3, 3
    sw   x3, 0(x2)
    li   x5, -1                 # 0xFFFFFFFF  (huge unsigned)
    amominu.w x6, x5, (x2)     # minu(3, 0xFFFFFFFF) = 3;  x6 = old (3), mem = 3

    lw   x7, 0(x2)
    li   x8, 3
    bne  x6, x8, .fail10
    bne  x7, x8, .fail10       # mem should still be 3 (was the minimum)
    j    .test11
.fail10:
    ori  x1, x1, 512            # set bit 9

    # =====================================================================
    # Test 11: AMOMAXU.W  (unsigned)
    # =====================================================================
.test11:
    li   x3, 3
    sw   x3, 0(x2)
    li   x5, -1                 # 0xFFFFFFFF  (huge unsigned)
    amomaxu.w x6, x5, (x2)     # maxu(3, 0xFFFFFFFF) = 0xFFFFFFFF;  x6 = old, mem = 0xFFFFFFFF

    lw   x7, 0(x2)
    li   x8, 3
    li   x9, -1
    bne  x6, x8, .fail11
    bne  x7, x9, .fail11
    j    .done
.fail11:
    ori  x1, x1, 1024           # set bit 10

    # =====================================================================
    # Done — x1 == 0 means all tests passed
    # =====================================================================
.done:
    # infinite loop (sim watches x1)
1:  j    1b
