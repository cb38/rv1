# ─────────────────────────────────────────────────────────────────────────────
# Misaligned load/store exception test — cxxrtl / tb_rv version
#
# Called from init_rv.S as: main()  (RISC-V C ABI)
# Returns:  a0 == 0  → PASS,  a0 != 0 → FAIL (bit N = sub-test N+1 failed)
#
# Tests:
#   1. Misaligned LH  → mcause=4, mtval=odd address
#   2. Misaligned LW  → mcause=4, mtval=unaligned address
#   3. Misaligned SH  → mcause=6, mtval=odd address
#   4. Misaligned SW  → mcause=6, mtval=unaligned address
#   5. Aligned accesses still work (no spurious trap)
# ─────────────────────────────────────────────────────────────────────────────

.section .text
.global main
main:
    li   a0, 0                  # error accumulator (0 = all pass)

    # ── Install trap handler at mtvec ────────────────────────────────────
    la   t0, trap_handler
    csrw mtvec, t0

    # Base address for scratch memory
    lui  a1, 0x80001            # a1 = 0x80001000

    # =====================================================================
    # Test 1: Misaligned LH (halfword load from odd address)
    #   Expect: mcause=4, mtval=a1+1
    # =====================================================================
    li   s0, 0                  # s0 = handler called flag
    li   s1, 0                  # s1 = mcause from handler
    li   s2, 0                  # s2 = mtval from handler
    addi a2, a1, 1              # a2 = odd address
    lh   a3, 0(a2)              # should trap (misaligned halfword)
    # If we reach here, handler advanced mepc past the faulting instr
    li   t0, 1
    bne  s0, t0, .fail1         # handler not called?
    li   t0, 4
    bne  s1, t0, .fail1         # mcause != 4?
    bne  s2, a2, .fail1         # mtval != faulting addr?
    j    .test2
.fail1:
    ori  a0, a0, 1

    # =====================================================================
    # Test 2: Misaligned LW (word load from address+1)
    #   Expect: mcause=4, mtval=a1+1
    # =====================================================================
.test2:
    li   s0, 0
    li   s1, 0
    li   s2, 0
    addi a2, a1, 1
    lw   a3, 0(a2)              # should trap
    li   t0, 1
    bne  s0, t0, .fail2
    li   t0, 4
    bne  s1, t0, .fail2
    bne  s2, a2, .fail2
    j    .test3
.fail2:
    ori  a0, a0, 2

    # =====================================================================
    # Test 3: Misaligned SH (halfword store to odd address)
    #   Expect: mcause=6, mtval=a1+3
    # =====================================================================
.test3:
    li   s0, 0
    li   s1, 0
    li   s2, 0
    addi a2, a1, 3              # odd address
    sh   a3, 0(a2)              # should trap
    li   t0, 1
    bne  s0, t0, .fail3
    li   t0, 6
    bne  s1, t0, .fail3
    bne  s2, a2, .fail3
    j    .test4
.fail3:
    ori  a0, a0, 4

    # =====================================================================
    # Test 4: Misaligned SW (word store to address+2)
    #   Expect: mcause=6, mtval=a1+2
    # =====================================================================
.test4:
    li   s0, 0
    li   s1, 0
    li   s2, 0
    addi a2, a1, 2
    sw   a3, 0(a2)              # should trap
    li   t0, 1
    bne  s0, t0, .fail4
    li   t0, 6
    bne  s1, t0, .fail4
    bne  s2, a2, .fail4
    j    .test5
.fail4:
    ori  a0, a0, 8

    # =====================================================================
    # Test 5: Aligned accesses still work (no spurious trap)
    # =====================================================================
.test5:
    li   s0, 0                  # clear handler flag
    li   t0, 0xDEAD
    sw   t0, 0(a1)              # aligned word store
    lw   t1, 0(a1)              # aligned word load
    bne  s0, zero, .fail5       # handler should NOT have been called
    bne  t1, t0, .fail5         # loaded value must match
    sh   t0, 4(a1)              # aligned halfword store
    lh   t1, 4(a1)              # aligned halfword load
    bne  s0, zero, .fail5
    j    .done
.fail5:
    ori  a0, a0, 16

.done:
    ret                         # return a0 to init_rv → _exit

# ─────────────────────────────────────────────────────────────────────────────
# Trap handler
#   Saves diagnostics for verification:
#     s0 = 1          (handler was called)
#     s1 = mcause
#     s2 = mtval
#   Advances mepc past the faulting instruction (4 bytes for 32-bit instr,
#   2 bytes for compressed) and returns via mret.
# ─────────────────────────────────────────────────────────────────────────────
.align 4
trap_handler:
    li   s0, 1                  # flag: handler entered
    csrr s1, mcause             # save mcause
    csrr s2, mtval              # save mtval (faulting address)
    csrr s3, mepc               # get faulting PC
    # Check if faulting instruction is compressed (16-bit)
    lhu  s4, 0(s3)              # load low 16 bits of instruction
    andi s4, s4, 3              # bits[1:0]
    li   s5, 3
    bne  s4, s5, .compressed    # if bits[1:0] != 11, it's a 16-bit instr
    addi s3, s3, 4              # skip 32-bit instruction
    j    .handler_done
.compressed:
    addi s3, s3, 2              # skip 16-bit instruction
.handler_done:
    csrw mepc, s3
    mret
