# ─────────────────────────────────────────────────────────────────────────────
# ECALL smoke test — cxxrtl / tb_rv version
#
# Called from init_rv.S as: main()  (RISC-V C ABI)
# Returns:  a0 == 0  → PASS,  a0 != 0 → FAIL (bit N = sub-test N+1 failed)
#
# Tests:
#   1. ecall jumps to mtvec handler
#   2. mcause == 11  (environment call from M-mode)
#   3. mepc  == PC of the ecall instruction
#   4. mstatus: MIE cleared, MPIE = old MIE, MPP = 11
#   5. mret resumes after ecall
# ─────────────────────────────────────────────────────────────────────────────

.section .text
.global main
main:
    li   a0, 0                  # error accumulator (0 = all pass)

    # ── Install trap handler at mtvec ────────────────────────────────────
    la   t0, trap_handler
    csrw mtvec, t0

    # ── Enable MIE in mstatus so we can verify MPIE is saved ────────────
    csrsi mstatus, 0x8          # set mstatus.MIE (bit 3)

    # ── Save the expected mepc (address of the ecall instruction) ────────
    la   t1, ecall_site
    # t1 = expected mepc

    # ── Flag: trap_handler sets t2 = 1 when called ──────────────────────
    li   t2, 0

    # ── Execute ecall ────────────────────────────────────────────────────
ecall_site:
    ecall
    # After mret we land here:
after_ecall:

    # =====================================================================
    # Test 1: trap handler was entered (t2 == 1)
    # =====================================================================
    li   t0, 1
    beq  t2, t0, .test2
    ori  a0, a0, 1              # bit 0: handler not called

    # =====================================================================
    # Test 2: mcause == 11
    # =====================================================================
.test2:
    li   t0, 11
    beq  t3, t0, .test3
    ori  a0, a0, 2              # bit 1: wrong mcause

    # =====================================================================
    # Test 3: mepc == ecall_site
    # =====================================================================
.test3:
    beq  t4, t1, .test4
    ori  a0, a0, 4              # bit 2: wrong mepc

    # =====================================================================
    # Test 4: mstatus after ecall:  MIE=0, MPIE=1, MPP=11
    #   bits tested: bit3=MIE(0), bit7=MPIE(1), bits12:11=MPP(11)
    #   mask = 0x1888  → expected = 0x1880
    # =====================================================================
.test4:
    li   t0, 0x1888
    and  t0, t5, t0             # t5 = mstatus snapshot from handler
    li   t1, 0x1880
    beq  t0, t1, .done
    ori  a0, a0, 8              # bit 3: wrong mstatus

.done:
    ret                         # return a0 to init_rv → _exit

# ─────────────────────────────────────────────────────────────────────────────
# Trap handler — entered via ecall
#   Saves diagnostics into callee-saved-safe temps for verification:
#     t2 = 1          (handler was called)
#     t3 = mcause
#     t4 = mepc
#     t5 = mstatus
#   Then advances mepc past the ecall (4 bytes) and returns via mret.
# ─────────────────────────────────────────────────────────────────────────────
.align 4
trap_handler:
    li   t2, 1                  # flag: handler entered
    csrr t3, mcause             # save mcause for test
    csrr t4, mepc               # save mepc for test (original value)
    csrr t5, mstatus            # save mstatus for test
    addi t6, t4, 4              # advance mepc past ecall (always 4 bytes)
    csrw mepc, t6               # write back advanced mepc (t4 preserved)
    mret
