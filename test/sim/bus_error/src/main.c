#include <stdint.h>
#include "tb_cxxrtl_io.h"
#include "rv_csr.h"

#define MCAUSE_LOAD_ACCESS_FAULT  5u
#define MCAUSE_STORE_ACCESS_FAULT 7u

volatile uint32_t g_trap_mcause = 0;
volatile uint32_t g_trap_count  = 0;

// Trap handler for both synchronous exceptions and interrupts.
// Advances mepc by 4 to skip the faulting instruction (all rv32im insns are 32-bit).
__attribute__((interrupt("machine"))) void trap_handler(void) {
    g_trap_mcause = read_csr(mcause);
    g_trap_count++;
    // Only advance mepc for synchronous exceptions (bit 31 clear)
    if (!(g_trap_mcause & 0x80000000u)) {
        write_csr(mepc, read_csr(mepc) + 4);
    }
}

static inline void fail(uint32_t code) {
    tb_puts("[bus_error] FAIL ");
    tb_put_u32(code);
    tb_exit(code);
}

int main(void) {
    tb_puts("[bus_error] start\n");

    write_csr(mtvec, (uint32_t)trap_handler);

    // --- Test 1: load from wholly unmapped address → mcause = 5 ---
    volatile uint32_t *unmapped = (volatile uint32_t *)0x10000000u;
    g_trap_mcause = 0;
    g_trap_count  = 0;
    (void)*unmapped;
    if (g_trap_count != 1u)                          fail(1u);
    if (g_trap_mcause != MCAUSE_LOAD_ACCESS_FAULT)   fail(2u);

    // --- Test 2: store to wholly unmapped address → mcause = 7 ---
    g_trap_mcause = 0;
    g_trap_count  = 0;
    *unmapped = 0xdeadbeefu;
    if (g_trap_count != 1u)                          fail(3u);
    if (g_trap_mcause != MCAUSE_STORE_ACCESS_FAULT)  fail(4u);

    // --- Test 3: poison a valid memory address, load → mcause = 5 ---
    volatile uint32_t *poison_target = (volatile uint32_t *)0x80002040u;
    mm_io->poison_addr = 0x80002040u;
    g_trap_mcause = 0;
    g_trap_count  = 0;
    (void)*poison_target;
    mm_io->poison_addr = 0xFFFFFFFCu;  // restore to default (unreachable addr)
    if (g_trap_count != 1u)                          fail(5u);
    if (g_trap_mcause != MCAUSE_LOAD_ACCESS_FAULT)   fail(6u);

    // --- Test 4: poison a valid memory address, store → mcause = 7 ---
    mm_io->poison_addr = 0x80002040u;
    g_trap_mcause = 0;
    g_trap_count  = 0;
    *poison_target = 0x12345678u;
    mm_io->poison_addr = 0xFFFFFFFCu;
    if (g_trap_count != 1u)                          fail(7u);
    if (g_trap_mcause != MCAUSE_STORE_ACCESS_FAULT)  fail(8u);

    tb_puts("[bus_error] PASS\n");
    tb_exit(0u);
    return 0;
}
