#include <stdint.h>
#include "tb_cxxrtl_io.h"
#include "hazard3_csr.h"

// Global status captured by the IRQ handler and checked in main.
volatile uint32_t g_ext_irq_count = 0;
volatile uint32_t g_timer_irq_count = 0;
volatile uint32_t g_irq_bad_cause = 0;

#define MCAUSE_MACHINE_TIMER_IRQ    0x80000007u
#define MCAUSE_MACHINE_EXTERNAL_IRQ 0x8000000Bu

static inline uint64_t tb_read_mtime64(void) {
    uint32_t hi0;
    uint32_t lo;
    uint32_t hi1;
    do {
        hi0 = mm_timer->mtimeh;
        lo = mm_timer->mtime;
        hi1 = mm_timer->mtimeh;
    } while (hi0 != hi1);
    return ((uint64_t)hi1 << 32) | lo;
}

static inline void tb_write_mtimecmp64(uint64_t value) {
    mm_timer->mtimecmp = 0xffffffffu;
    mm_timer->mtimecmph = (uint32_t)(value >> 32);
    mm_timer->mtimecmp = (uint32_t)value;
}

// Unified failure path: print a tag + numeric code, then exit testbench.
static inline void fail(uint32_t code) {
    tb_puts("[irq_csr] FAIL\n");
    tb_put_u32(code);
    tb_exit(code);
}

// Machine interrupt handler used to validate IRQ entry + mcause content.
__attribute__((interrupt("machine"))) void irq_handler(void) {
    uint32_t cause = read_csr(mcause);
    if (cause == MCAUSE_MACHINE_EXTERNAL_IRQ) {
        g_ext_irq_count++;
        tb_clr_irq_masked(1u);
    } else if (cause == MCAUSE_MACHINE_TIMER_IRQ) {
        g_timer_irq_count++;
        tb_write_mtimecmp64(~0ull);
    } else {
        g_irq_bad_cause = cause;
    }
}

int main(void) {
    tb_puts("[irq_csr] start\n");

    // Route all traps/interrupts to our local handler.
    write_csr(mtvec, (uint32_t)irq_handler);
    tb_write_mtimecmp64(~0ull);

    // Basic CSR R/W validation on mscratch.
    write_csr(mscratch, 0x11223344u);
    if (read_csr(mscratch) != 0x11223344u) fail(1u);

    // Validate CSRRW behavior: return previous value and update CSR.
    uint32_t prev = read_write_csr(mscratch, 0x55667788u);
    if (prev != 0x11223344u) fail(2u);
    if (read_csr(mscratch) != 0x55667788u) fail(3u);

    // Validate CSR set/clear operations on mstatus.MIE (bit 3).
    write_csr(mstatus, 0u);
    set_csr(mstatus, (1u << 3));
    if ((read_csr(mstatus) & (1u << 3)) == 0u) fail(4u);
    clear_csr(mstatus, (1u << 3));
    if (read_csr(mstatus) & (1u << 3)) fail(5u);

    // Validate direct R/W on mepc and mcause.
    write_csr(mepc, 0x80000100u);
    if (read_csr(mepc) != 0x80000100u) fail(6u);

    write_csr(mcause, 0x123u);
    if (read_csr(mcause) != 0x123u) fail(7u);

    // Enable machine external interrupt path: mie.MEIE + mstatus.MIE.
    write_csr(mie, 0u);
    set_csr(mie, (1u << 11));
    set_csr(mstatus, (1u << 3));

    // Trigger external IRQ from testbench MMIO side.
    tb_set_irq_masked(1u);

    // Wait bounded time for interrupt service.
    uint32_t timeout = 200000u;
    while (g_ext_irq_count == 0u && timeout--) {
        asm volatile("nop");
    }

    // Checks for external IRQ.
    if (g_ext_irq_count == 0u) fail(8u);
    if (g_irq_bad_cause != 0u) fail(9u);
    if (read_csr(mcause) != MCAUSE_MACHINE_EXTERNAL_IRQ) fail(10u);

    // Program a near-future timer deadline and enable machine timer IRQ.
    tb_clr_irq_masked(1u);
    uint64_t now = tb_read_mtime64();
    tb_write_mtimecmp64(now + 200ull);
    write_csr(mie, 0u);
    set_csr(mie, (1u << 7));
    if ((read_csr(mie) & (1u << 7)) == 0u) fail(14u);
    if (read_csr(mie) & (1u << 11)) fail(15u);
    clear_csr(mstatus, (1u << 3));

    uint32_t pending_timeout = 200000u;
    while (((read_csr(mip) & (1u << 7)) == 0u) && (pending_timeout != 0u)) {
        pending_timeout--;
        asm volatile("nop");
    }
    if ((read_csr(mip) & (1u << 7)) == 0u) fail(16u);

    set_csr(mstatus, (1u << 3));

    timeout = 200000u;
    while (g_timer_irq_count == 0u && timeout--) {
        asm volatile("nop");
    }

    // Checks for timer IRQ.
    if (g_timer_irq_count == 0u) fail(11u);
    if (g_irq_bad_cause != 0u) fail(12u);
    if (read_csr(mcause) != MCAUSE_MACHINE_TIMER_IRQ) fail(13u);
    if (read_csr(mip) & (1u << 7)) fail(17u);

    tb_puts("[irq_csr] PASS\n");
    return 0;
}
