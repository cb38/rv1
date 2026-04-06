#include "tb.h"

#include <fstream>
#include <iostream>

// -----------------------------------------------------------------------
// UART 8250 shim — minimal model for Linux earlycon/8250 driver
// -----------------------------------------------------------------------
static uint8_t  uart_ier = 0;
static uint8_t  uart_lcr = 0;
static uint8_t  uart_mcr = 0;
static uint8_t  uart_scr = 0;
static uint8_t  uart_dll = 0;  // divisor latch low  (accessible when LCR.DLAB=1)
static uint8_t  uart_dlh = 0;  // divisor latch high

static uint32_t uart_read(uint32_t offset) {
	bool dlab = (uart_lcr >> 7) & 1;
	switch (offset) {
		case UART_THR: return dlab ? uart_dll : 0x00;    // RBR: no input
		case UART_IER: return dlab ? uart_dlh : uart_ier;
		case UART_IIR: return 0x01;                       // no pending interrupt
		case UART_LCR: return uart_lcr;
		case UART_MCR: return uart_mcr;
		case UART_LSR: return 0x60;                       // THRE + TEMT = TX always ready
		case UART_MSR: return 0x00;
		case UART_SCR: return uart_scr;
		default:       return 0;
	}
}

static void uart_write(FILE *logfile, uint32_t offset, uint32_t data) {
	bool dlab = (uart_lcr >> 7) & 1;
	switch (offset) {
		case UART_THR:
			if (dlab) { uart_dll = data & 0xff; }
			else { fputc(data & 0xff, logfile); fflush(logfile); }
			break;
		case UART_IER: if (dlab) uart_dlh = data & 0xff; else uart_ier = data & 0xff; break;
		case UART_IIR: break;  // FCR write — ignore
		case UART_LCR: uart_lcr = data & 0xff; break;
		case UART_MCR: uart_mcr = data & 0xff; break;
		case UART_LSR: break;  // read-only
		case UART_MSR: break;  // read-only
		case UART_SCR: uart_scr = data & 0xff; break;
	}
}

mem_io_state::mem_io_state(const tb_cli_args &args) {
	mtime = 0;
	mtimecmp[0] = 0;
	mtimecmp[1] = 0;
	exit_req = false;
	exit_code = 0;
	monitor_enabled = false;
	soft_irq_state = 0;
	irq_state = 0;
	for (int i = 0; i < N_RESERVATIONS; ++i) {
		reservation_valid[i] = false;
		reservation_addr[i] = 0;
	}
	poison_addr = -4u;
	gpio_a_in = 0;
	gpio_b_out = 0;
	gpio_b_oe = 0;
	permissive_io = args.noshift;  // Enable permissive I/O in Linux mode
	mem = new uint8_t[MEM_SIZE];
	for (size_t i = 0; i < MEM_SIZE; ++i)
		mem[i] = 0;

	if (args.load_bin) {
		std::ifstream fd(args.bin_path, std::ios::binary | std::ios::ate);
		if (!fd){
			std::cerr << "Failed to open \"" << args.bin_path << "\"\n";
			exit(-1);
		}
		std::streamsize bin_size = fd.tellg();
		if (bin_size > MEM_SIZE) {
			std::cerr << "Binary file (" << bin_size << " bytes) is larger than memory (" << MEM_SIZE << " bytes)\n";
			exit(-1);
		}
		fd.seekg(0, std::ios::beg);
		fd.read((char*)mem, bin_size);
	}
}

bus_response tb_mem_access(tb_top &tb, mem_io_state &memio, bus_request req) {
	bus_response resp;

	// Global monitor. When monitor is not enabled, HEXOKAY is tied high
	if (memio.monitor_enabled) {
		if (req.excl) {
			// Always set reservation on read. Always clear reservation on
			// write. On successful write, clear others' matching reservations.
			if (req.write) {
				resp.exokay = memio.reservation_valid[req.reservation_id] &&
					memio.reservation_addr[req.reservation_id] == (req.addr & RESERVATION_ADDR_MASK);
				memio.reservation_valid[req.reservation_id] = false;
				if (resp.exokay) {
					for (int i = 0; i < N_RESERVATIONS; ++i) {
						if (i == req.reservation_id)
							continue;
						if (memio.reservation_addr[i] == (req.addr & RESERVATION_ADDR_MASK))
							memio.reservation_valid[i] = false;
					}
				}
			} else {
				resp.exokay = true;
				memio.reservation_valid[req.reservation_id] = true;
				memio.reservation_addr[req.reservation_id] = req.addr & RESERVATION_ADDR_MASK;
			}
		} else {
			resp.exokay = false;
			// Non-exclusive write still clears others' reservations
			if (req.write) {
				for (int i = 0; i < N_RESERVATIONS; ++i) {
					if (i == req.reservation_id)
						continue;
					if (memio.reservation_addr[i] == (req.addr & RESERVATION_ADDR_MASK))
						memio.reservation_valid[i] = false;
				}
			}
		}
	}


	if (req.write) {
		if (memio.monitor_enabled && req.excl && !resp.exokay) {
			// Failed exclusive write; do nothing
		} else if ((req.addr & -4u) == memio.poison_addr) {
			resp.err = true;
		} else if (req.addr >= MEM_BASE && req.addr <= MEM_BASE + MEM_SIZE - (1u << (int)req.size)) {
			unsigned int n_bytes = 1u << (int)req.size;
			// Note we are relying on rv's byte lane replication
			for (unsigned int i = 0; i < n_bytes; ++i) {
				memio.mem[req.addr + i - MEM_BASE] = req.wdata >> (8 * i) & 0xffu;
			}
		} else if (req.addr == IO_BASE + IO_PRINT_CHAR) {
			fprintf(tb.logfile, "%c", (char)(req.wdata & 0xff));
		} else if (req.addr == IO_BASE + IO_PRINT_U32) {
			fprintf(tb.logfile, "%08x\n", req.wdata);
		} else if (req.addr == IO_BASE + IO_EXIT) {
			if (!memio.exit_req) {
				memio.exit_req = true;
				memio.exit_code = req.wdata;
			}
		} else if (req.addr == IO_BASE + IO_SET_SOFTIRQ) {
			memio.soft_irq_state |= req.wdata;
			tb.set_soft_irq(memio.soft_irq_state);
		} else if (req.addr == IO_BASE + IO_CLR_SOFTIRQ) {
			memio.soft_irq_state &= ~req.wdata;
			tb.set_soft_irq(memio.soft_irq_state);
		} else if (req.addr == IO_BASE + IO_GLOBMON_EN) {
			memio.monitor_enabled = req.wdata;
		} else if (req.addr == IO_BASE + IO_POISON_ADDR) {
			memio.poison_addr = req.wdata & -4u;
		} else if (req.addr == IO_BASE + IO_SET_IRQ) {
			memio.irq_state |= req.wdata;
			tb.set_irq(memio.irq_state);
		} else if (req.addr == IO_BASE + IO_CLR_IRQ) {
			memio.irq_state &= ~req.wdata;
			tb.set_irq(memio.irq_state);
		} else if (req.addr == IO_BASE + IO_MTIME) {
			memio.mtime = (memio.mtime & 0xffffffff00000000u) | req.wdata;
		} else if (req.addr == IO_BASE + IO_MTIMEH) {
			memio.mtime = (memio.mtime & 0x00000000ffffffffu) | ((uint64_t)req.wdata << 32);
		} else if (req.addr == IO_BASE + IO_MTIMECMP0) {
			memio.mtimecmp[0] = (memio.mtimecmp[0] & 0xffffffff00000000u) | req.wdata;
		} else if (req.addr == IO_BASE + IO_MTIMECMP0H) {
			memio.mtimecmp[0] = (memio.mtimecmp[0] & 0x00000000ffffffffu) | ((uint64_t)req.wdata << 32);
		} else if (req.addr == IO_BASE + IO_MTIMECMP1) {
			memio.mtimecmp[1] = (memio.mtimecmp[1] & 0xffffffff00000000u) | req.wdata;
		} else if (req.addr == IO_BASE + IO_MTIMECMP1H) {
			memio.mtimecmp[1] = (memio.mtimecmp[1] & 0x00000000ffffffffu) | ((uint64_t)req.wdata << 32);
		// ---- UART 8250 write ----
		} else if (req.addr >= UART_BASE && req.addr < UART_BASE + 0x20) {
			uart_write(tb.logfile, req.addr - UART_BASE, req.wdata);
		} else if (req.addr >= UART_VIRT_BASE && req.addr < UART_VIRT_BASE + 8) {
			uart_write(tb.logfile, (req.addr - UART_VIRT_BASE) * 4, req.wdata);
		// ---- CLINT standard layout write ----
		} else if (req.addr == CLINT_BASE + CLINT_MSIP) {
			memio.soft_irq_state = req.wdata & 1;
			tb.set_soft_irq(memio.soft_irq_state);
		} else if (req.addr == CLINT_BASE + CLINT_MTIMECMP) {
			memio.mtimecmp[0] = (memio.mtimecmp[0] & 0xffffffff00000000u) | req.wdata;
		} else if (req.addr == CLINT_BASE + CLINT_MTIMECMPH) {
			memio.mtimecmp[0] = (memio.mtimecmp[0] & 0x00000000ffffffffu) | ((uint64_t)req.wdata << 32);
		} else if (req.addr == CLINT_BASE + CLINT_MTIME) {
			memio.mtime = (memio.mtime & 0xffffffff00000000u) | req.wdata;
		} else if (req.addr == CLINT_BASE + CLINT_MTIMEH) {
			memio.mtime = (memio.mtime & 0x00000000ffffffffu) | ((uint64_t)req.wdata << 32);
		} else {
			// Unmapped write
			if (memio.permissive_io) {
				static int unmapped_wr_count = 0;
				if (unmapped_wr_count < 5) {
					fprintf(stderr, "[UNMAPPED_WR] addr=0x%08x data=0x%08x size=%d\n",
						req.addr, req.wdata, (int)req.size);
					++unmapped_wr_count;
				}
			} else {
				resp.err = true;
			}
		}
	} else {
		if (req.addr == (memio.poison_addr & -4u)) {
			resp.err = true;
		} else if (req.addr >= MEM_BASE && req.addr <= MEM_BASE + MEM_SIZE - (1u << (int)req.size)) {
			req.addr &= ~0x3u;
			req.addr -= MEM_BASE;
			resp.rdata =
				(uint32_t)memio.mem[req.addr] |
				memio.mem[req.addr + 1] << 8 |
				memio.mem[req.addr + 2] << 16 |
				memio.mem[req.addr + 3] << 24;
		} else if (req.addr == IO_BASE + IO_SET_SOFTIRQ || req.addr == IO_BASE + IO_CLR_SOFTIRQ) {
			resp.rdata = memio.soft_irq_state;
		} else if (req.addr == IO_BASE + IO_SET_IRQ || req.addr == IO_BASE + IO_CLR_IRQ) {
			resp.rdata = memio.irq_state;
		} else if (req.addr == IO_BASE + IO_MTIME) {
			resp.rdata = memio.mtime;
		} else if (req.addr == IO_BASE + IO_MTIMEH) {
			resp.rdata = memio.mtime >> 32;
		} else if (req.addr == IO_BASE + IO_MTIMECMP0) {
			resp.rdata = memio.mtimecmp[0];
		} else if (req.addr == IO_BASE + IO_MTIMECMP0H) {
			resp.rdata = memio.mtimecmp[0] >> 32;
		} else if (req.addr == IO_BASE + IO_MTIMECMP1) {
			resp.rdata = memio.mtimecmp[1];
		} else if (req.addr == IO_BASE + IO_MTIMECMP1H) {
			resp.rdata = memio.mtimecmp[1] >> 32;
		// ---- UART 8250 read ----
		} else if (req.addr >= UART_BASE && req.addr < UART_BASE + 0x20) {
			resp.rdata = uart_read(req.addr - UART_BASE);
		} else if (req.addr >= UART_VIRT_BASE && req.addr < UART_VIRT_BASE + 8) {
			uint32_t val = uart_read((req.addr - UART_VIRT_BASE) * 4) & 0xff;
			resp.rdata = val | (val << 8) | (val << 16) | (val << 24);
		// ---- CLINT standard layout read ----
		} else if (req.addr == CLINT_BASE + CLINT_MSIP) {
			resp.rdata = memio.soft_irq_state & 1;
		} else if (req.addr == CLINT_BASE + CLINT_MTIMECMP) {
			resp.rdata = memio.mtimecmp[0];
		} else if (req.addr == CLINT_BASE + CLINT_MTIMECMPH) {
			resp.rdata = memio.mtimecmp[0] >> 32;
		} else if (req.addr == CLINT_BASE + CLINT_MTIME) {
			resp.rdata = memio.mtime;
		} else if (req.addr == CLINT_BASE + CLINT_MTIMEH) {
			resp.rdata = memio.mtime >> 32;
		} else {
			// Unmapped read
			if (memio.permissive_io) {
				resp.rdata = 0;
				static int unmapped_rd_count = 0;
				if (unmapped_rd_count < 5) {
					fprintf(stderr, "[UNMAPPED_RD] addr=0x%08x (returning 0)\n", req.addr);
					++unmapped_rd_count;
				}
			} else {
				resp.err = true;
			}
		}
	}
	if (resp.err) {
		resp.exokay = false;
		static int err_count = 0;
		if (err_count < 50) {
			fprintf(stderr, "[BUS_ERR] %s addr=0x%08x data=0x%08x size=%d\n",
				req.write ? "WR" : "RD", req.addr, req.write ? req.wdata : resp.rdata, (int)req.size);
			++err_count;
		}
	}
	return resp;
}

void mem_io_state::step(tb_top &tb) {
	// Default update logic for mtime, mtimecmp
	++mtime;
	uint8_t new_tirq = (uint8_t)((mtime >= mtimecmp[0]) | (mtime >= mtimecmp[1]) << 1);
	tb.set_timer_irq(new_tirq);
}
