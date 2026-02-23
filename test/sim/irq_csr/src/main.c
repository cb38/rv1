#include <stdint.h>
#include "tb_cxxrtl_io.h"
#include "hazard3_csr.h"

// Global status captured by the IRQ handler and checked in main.
volatile uint32_t g_irq_count = 0;
volatile uint32_t g_irq_bad_cause = 0;

// Unified failure path: print a tag + numeric code, then exit testbench.
static inline void fail(uint32_t code) {
    tb_puts("[irq_csr] FAIL\n");
    tb_put_u32(code);
    tb_exit(code);
}

// Machine interrupt handler used to validate IRQ entry + mcause content.
__attribute__((interrupt("machine"))) void irq_handler(void) {
    uint32_t cause = read_csr(mcause);
    // Expected external machine interrupt cause bit pattern.
    if (cause != 0x8000000Bu) {
        g_irq_bad_cause = cause;
    }
    // Count handled IRQs and clear the pending testbench IRQ source.
    g_irq_count++;
    tb_clr_irq_masked(1u);
}

int main(void) {
    tb_puts("[irq_csr] start\n");

    // Route all traps/interrupts to our local handler.
    write_csr(mtvec, (uint32_t)irq_handler);

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
    while (g_irq_count == 0u && timeout--) {
        asm volatile("nop");
    }

    // Final checks: IRQ observed, cause value correct, mcause latched.
    if (g_irq_count == 0u) fail(8u);
    if (g_irq_bad_cause != 0u) fail(9u);
    if (read_csr(mcause) != 0x8000000Bu) fail(10u);

    tb_puts("[irq_csr] PASS\n");
    return 0;
}
