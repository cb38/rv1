/*
 * float_printf test: investigates why ee_printf(%f) fails.
 * Uses ee_printf.c from coremark barebones directly.
 * Tests: %d/%s (no libc), soft-float arith (libgcc), %f/%e (fcvtbuf from libc)
 */
#include <stdint.h>

/* Minimal MMIO IO */
#define IO_PRINT_CHAR (*(volatile uint32_t *)0xc0000000u)

static void tb_putc(char c) { IO_PRINT_CHAR = (uint8_t)c; }
static void tb_puts(const char *s) { while (*s) tb_putc(*s++); }

static void tb_put_hex(uint32_t v) {
    const char *h = "0123456789abcdef";
    tb_puts("0x");
    for (int i = 28; i >= 0; i -= 4) tb_putc(h[(v >> i) & 0xf]);
}

static void tb_put_u32(uint32_t v) {
    char buf[12]; int i = 0;
    if (!v) { tb_putc('0'); return; }
    while (v) { buf[i++] = '0' + v % 10; v /= 10; }
    while (i--) tb_putc(buf[i]);
}

/* Trap handler */
static volatile uint32_t trap_count  = 0;
static volatile uint32_t trap_mcause = 0;
static volatile uint32_t trap_mepc   = 0;

__attribute__((interrupt("machine"), aligned(4)))
void trap_handler(void) {
    uint32_t cause, epc;
    asm volatile ("csrr %0, mcause" : "=r"(cause));
    asm volatile ("csrr %0, mepc"   : "=r"(epc));
    trap_mcause = cause; trap_mepc = epc; trap_count++;
    uint16_t insn = *(uint16_t *)epc;
    epc += ((insn & 0x3) == 0x3) ? 4 : 2;
    asm volatile ("csrw mepc, %0" : : "r"(epc));
}

static void setup_trap(void) {
    asm volatile ("csrw mtvec, %0" : : "r"((uint32_t)trap_handler & ~3u));
}

static inline uint32_t read_mcycle(void) {
    uint32_t v; asm volatile ("csrr %0, mcycle" : "=r"(v)); return v;
}

/* ee_printf from coremark (compiled separately) */
extern int ee_printf(const char *fmt, ...);

int main(void) {
    setup_trap();
    tb_puts("=== float_printf investigation ===\n");

    /* 0. Direct test of Zbs instructions vs SLLI baseline */
    { uint32_t result;
      /* SLLI a0, a0, 20: funct7=0000000 shamt=10100 rs1=01010 f3=001 rd=01010 opcode=0010011
         = 0000000_10100_01010_001_01010_0010011 = 0x01451513 */
      asm volatile ("li a0, 1\n\t"
                    "slli a0, a0, 20\n\t"
                    "mv %0, a0" : "=r"(result) :: "a0");
      tb_puts("  slli a0,1,20  = "); tb_put_hex(result);
      tb_puts(result == 0x100000 ? " OK\n" : " FAIL\n");

      /* ORI a0, zero, 1 then bseti a0, a0, 20 */
      /* bseti a0, a0, 20 : funct7=0010100 shamt=10100 rs1=01010 f3=001 rd=01010 opcode=0010011
         = 0010100_10100_01010_001_01010_0010011 = 0x29451513 */
      asm volatile ("li a0, 0\n\t"
                    ".word 0x29451513\n\t"
                    "mv %0, a0" : "=r"(result) :: "a0");
      tb_puts("  bseti a0,0,20 = "); tb_put_hex(result);
      tb_puts(result == 0x100000 ? " OK\n" : " FAIL\n");

      /* bseti a0, a0, 0 (set bit 0 on 0) */
      /* funct7=0010100 shamt=00000 rs1=01010 f3=001 rd=01010 opcode=0010011
         = 0010100_00000_01010_001_01010_0010011 = 0x28051513 */
      asm volatile ("li a0, 0\n\t"
                    ".word 0x28051513\n\t"
                    "mv %0, a0" : "=r"(result) :: "a0");
      tb_puts("  bseti a0,0,0  = "); tb_put_hex(result);
      tb_puts(result == 1 ? " OK\n" : " FAIL\n");

      /* bclri a0, a0, 31 : funct7=0100100 shamt=11111 rs1=01010 f3=001 rd=01010 opcode=0010011
         = 0100100_11111_01010_001_01010_0010011 = 0x49f51513 */
      asm volatile ("li a0, -1\n\t"
                    ".word 0x49f51513\n\t"
                    "mv %0, a0" : "=r"(result) :: "a0");
      tb_puts("  bclri a0,-1,31= "); tb_put_hex(result);
      tb_puts(result == 0x7FFFFFFF ? " OK\n" : " FAIL\n");

      /* Also test trap_count to see if instructions trapped */
      tb_puts("  trap_count    = "); tb_put_u32(trap_count);
      tb_puts(trap_count == 0 ? " OK\n" : " FAIL (traps!)\n");
    }

    /* 1. %d - ee_printf own integer path, no libc */
    { uint32_t tc=trap_count, t0=read_mcycle();
      ee_printf("  %%d -> %d\n", 42);
      uint32_t t1=read_mcycle();
      tb_puts("  [ee_printf %d] cycles="); tb_put_u32(t1-t0);
      tb_puts(" traps="); tb_put_u32(trap_count-tc); tb_puts("\n"); }

    /* 2. %s - ee_printf own string path, no libc */
    { uint32_t tc=trap_count, t0=read_mcycle();
      ee_printf("  %%s -> %s\n", "hello");
      uint32_t t1=read_mcycle();
      tb_puts("  [ee_printf %s] cycles="); tb_put_u32(t1-t0);
      tb_puts(" traps="); tb_put_u32(trap_count-tc); tb_puts("\n"); }

    /* 3. raw soft-float arithmetic only (libgcc __muldf3 etc.) */
    { uint32_t tc=trap_count, t0=read_mcycle();
      volatile double a=3.14159265358979, b=2.71828182845904;
      volatile double c = a * b + a / b;
      uint32_t t1=read_mcycle();
      tb_puts("  result*1000="); tb_put_u32((uint32_t)(c * 1000.0)); tb_puts("\n");
      tb_puts("  [soft-float arith] cycles="); tb_put_u32(t1-t0);
      tb_puts(" traps="); tb_put_u32(trap_count-tc); tb_puts("\n"); }

    /* 4. %f -> ee_printf -> parse_float -> fcvtbuf (libc) */
    { uint32_t tc=trap_count, t0=read_mcycle();
      ee_printf("  %%f -> %f\n", 3.14159265);
      uint32_t t1=read_mcycle();
      tb_puts("  [ee_printf %f] cycles="); tb_put_u32(t1-t0);
      tb_puts(" traps="); tb_put_u32(trap_count-tc); tb_puts("\n");
      if (trap_count-tc) {
          tb_puts("    mcause="); tb_put_hex(trap_mcause);
          tb_puts(" mepc=");     tb_put_hex(trap_mepc); tb_puts("\n"); } }

    /* 5. %e -> same path as %f */
    { uint32_t tc=trap_count, t0=read_mcycle();
      ee_printf("  %%e -> %e\n", 3.14159265);
      uint32_t t1=read_mcycle();
      tb_puts("  [ee_printf %e] cycles="); tb_put_u32(t1-t0);
      tb_puts(" traps="); tb_put_u32(trap_count-tc); tb_puts("\n");
      if (trap_count-tc) {
          tb_puts("    mcause="); tb_put_hex(trap_mcause);
          tb_puts(" mepc=");     tb_put_hex(trap_mepc); tb_puts("\n"); } }

    /* 6. Direct test of fcvtbuf from cvt.c */
    { char cvtbuf[80];
      int decpt, sign;
      extern char *fcvtbuf(double arg, int ndigits, int *decpt, int *sign, char *buf);
      char *digits = fcvtbuf(47.600059, 6, &decpt, &sign, cvtbuf);
      tb_puts("  fcvtbuf(47.600059, 6) -> digits=\""); tb_puts(digits);
      tb_puts("\" decpt="); tb_put_u32(decpt);
      tb_puts(" sign="); tb_put_u32(sign); tb_puts("\n");
      /* print raw bytes */
      tb_puts("  raw bytes: ");
      for (int i = 0; digits[i]; i++) {
          tb_put_hex((uint8_t)digits[i]); tb_puts(" ");
      }
      tb_puts("\n");
    }

    /* 7. Test modf directly */
    { extern double modf(double, double *);
      double ipart;
      double fpart = modf(47.600059, &ipart);
      tb_puts("  modf(47.600059) -> ipart="); tb_put_u32((uint32_t)ipart);
      tb_puts(" fpart*1e6="); tb_put_u32((uint32_t)(fpart * 1000000.0));
      tb_puts("\n");
    }

    /* 8. Dump raw bytes of a known double constant */
    { volatile double val = 47.600059;
      uint32_t *p = (uint32_t *)&val;
      tb_puts("  47.600059 raw: lo="); tb_put_hex(p[0]);
      tb_puts(" hi="); tb_put_hex(p[1]);
      tb_puts("\n");
      /* expected IEEE754: 0x4047E6697CC78E37 -> hi=4047E669 lo=7CC78E37 */
      volatile double val2 = 3.14159265358979;
      p = (uint32_t *)&val2;
      tb_puts("  pi raw:        lo="); tb_put_hex(p[0]);
      tb_puts(" hi="); tb_put_hex(p[1]);
      tb_puts("\n");
      /* Also test basic double->uint32 conversion */
      volatile double d50 = 50.0;
      tb_puts("  (uint32_t)50.0 = "); tb_put_u32((uint32_t)d50); tb_puts("\n");
      volatile double d100 = 100.0;
      tb_puts("  (uint32_t)100.0 = "); tb_put_u32((uint32_t)d100); tb_puts("\n");
      /* Test a simple multiply */
      volatile double a2 = 2.0, b2 = 3.0;
      volatile double c2 = a2 * b2;
      tb_puts("  2.0*3.0 raw: lo="); 
      p = (uint32_t *)&c2; tb_put_hex(p[0]);
      tb_puts(" hi="); tb_put_hex(p[1]);
      tb_puts("  as uint="); tb_put_u32((uint32_t)c2);
      tb_puts("\n");
    }

    tb_puts("=== done ===\n");
    return 0;
}
