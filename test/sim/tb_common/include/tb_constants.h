#pragma once

#ifdef __x86_64__
#define I64_FMT "%ld"
#else
#define I64_FMT "%lld"
#endif

#define MEM_BASE 0x80000000
#define MEM_SIZE (16 * 1024 * 1024)
#define N_RESERVATIONS (2)
#define RESERVATION_ADDR_MASK (0xfffffff8u)

static const unsigned int IO_BASE = 0xc0000000;
enum {
	IO_PRINT_CHAR  = 0x000,
	IO_PRINT_U32   = 0x004,
	IO_EXIT        = 0x008,
	IO_SET_SOFTIRQ = 0x010,
	IO_CLR_SOFTIRQ = 0x014,
	IO_GLOBMON_EN  = 0x018,
	IO_POISON_ADDR = 0x01c,
	IO_SET_IRQ     = 0x020,
	IO_CLR_IRQ     = 0x030,
	IO_GPIO_A_IN   = 0x040,  // W: set simulated GPIO-A input state (button state for tests)
	IO_MTIME       = 0x100,
	IO_MTIMEH      = 0x104,
	IO_MTIMECMP0   = 0x108,
	IO_MTIMECMP0H  = 0x10c,
	IO_MTIMECMP1   = 0x110,
	IO_MTIMECMP1H  = 0x114
};

// UART 8250 compatible registers at IO_BASE + 0x200
// Linux uses earlycon=uart8250,mmio,0xC0000200
static const unsigned int UART_BASE = 0xC0000200;
static const unsigned int UART_VIRT_BASE = 0x10000000;  // QEMU virt UART (byte-spaced, reg-shift=0)
enum {
	UART_THR = 0x00,  // Transmit Holding Register (W) / Receive Buffer (R)
	UART_IER = 0x04,  // Interrupt Enable Register
	UART_IIR = 0x08,  // Interrupt Identification (R) / FCR (W)
	UART_LCR = 0x0C,  // Line Control Register
	UART_MCR = 0x10,  // Modem Control Register
	UART_LSR = 0x14,  // Line Status Register
	UART_MSR = 0x18,  // Modem Status Register
	UART_SCR = 0x1C,  // Scratch Register
};

// CLINT at standard RISC-V address 0x02000000
// Layout: msip @+0x0000, mtimecmp @+0x4000, mtime @+0xBFF8
static const unsigned int CLINT_BASE = 0x02000000;
enum {
	CLINT_MSIP       = 0x0000,
	CLINT_MTIMECMP   = 0x4000,
	CLINT_MTIMECMPH  = 0x4004,
	CLINT_MTIME      = 0xBFF8,
	CLINT_MTIMEH     = 0xBFFC,
};

typedef enum {
	SIZE_BYTE = 0,
	SIZE_HWORD = 1,
	SIZE_WORD = 2
} bus_size_t;
