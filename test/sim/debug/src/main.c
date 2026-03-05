// Debug test program
// Sets up known register values, then spins in a loop.
// The debug testbench will:
//   1. Halt the core via DMI
//   2. Read GPRs and verify values
//   3. Write 0x42424242 to a3 (x13)
//   4. Resume the core
// After resume, this program checks a3 and exits with
// code 0 (PASS) or 1 (FAIL).

#include "tb_cxxrtl_io.h"

int main(void) {
    register volatile unsigned int a0_val __asm__("a0");
    register volatile unsigned int a1_val __asm__("a1");
    register volatile unsigned int a2_val __asm__("a2");
    register volatile unsigned int a3_val __asm__("a3");

    // Set known values in registers
    // Using inline asm to ensure the compiler doesn't optimize them away
    __asm__ volatile (
        "li a0, 0x12345678\n"
        "li a1, 0xDEADBEEF\n"   // note: actually -559038737
        "li a2, 0xCAFEBABE\n"   // note: actually -889275714
        "li a3, 0\n"
        ::: "a0", "a1", "a2", "a3"
    );

    // Spin waiting for debugger to write 0x42424242 to a3
    volatile unsigned int check;
    do {
        __asm__ volatile ("mv %0, a3" : "=r"(check));
    } while (check != 0x42424242u);

    // Debugger wrote the expected value - PASS
    return 0;
}
