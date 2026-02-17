#include "Vtb.h"
#include "verilated.h"

#include <iostream>
#include <cstdint>
#include <cstring>
#include <string>
#include <stdio.h>

#include "tb.h"
#include "tb_cli.h"
#include "tb_jtag.h"

class tb_verilator_top: public tb_top {
	VerilatedContext *contextp;
	Vtb *top;

	uint64_t cycle = 0;

	bool d_r_pending_valid = false;
	uint32_t d_r_pending_data = 0;
	uint32_t d_r_pending_resp = 0;

	bool aw_pending = false;
	uint32_t aw_addr = 0;
	bool w_pending = false;
	uint32_t w_data = 0;
	uint32_t w_strb = 0;

public:
	static constexpr uint32_t TB_BOOT_PC = 0x80000040u;

	tb_verilator_top(const tb_cli_args &parsed_args, int argc, char **argv);
	~tb_verilator_top() {delete top; delete contextp;}

	void step(const tb_cli_args &args, mem_io_state &memio) override;

	void set_trst_n(bool trst_n)     override {top->trst_n = trst_n;}
	void set_tck(bool tck)           override {top->tck = tck;}
	void set_tdi(bool tdi)           override {top->tdi = tdi;}
	void set_tms(bool tms)           override {top->tms = tms;}
	bool get_tdo()                   override {return top->tdo;}
	void set_irq(uint32_t mask)      override {top->irq = (mask != 0);}
	void set_soft_irq(uint8_t mask)  override {top->soft_irq = mask & 0x3u;}
	void set_timer_irq(uint8_t mask) override {top->timer_irq = mask & 0x3u;}
};

tb_verilator_top::tb_verilator_top(const tb_cli_args &parsed_args, int argc, char **argv): tb_top {parsed_args} {
	contextp = new VerilatedContext;
	contextp->commandArgs(argc, argv);
	top = new Vtb{contextp};

	top->i_axi_ar_ready = true;
	top->i_axi_r_valid = false;
	top->i_axi_r_data = 0;
	top->i_axi_r_resp = 0;

	top->d_axi_aw_ready = true;
	top->d_axi_w_ready = false;
	top->d_axi_b_valid = false;
	top->d_axi_b_resp = 0;
	top->d_axi_ar_ready = true;
	top->d_axi_r_valid = false;
	top->d_axi_r_data = 0;
	top->d_axi_r_resp = 0;

	top->irq = false;
	top->soft_irq = 0;
	top->timer_irq = 0;

	top->rst_n = false;
	top->eval();
	top->clk = true;
	top->tck = true;
	top->eval();
	top->clk = false;
	top->tck = false;
	top->trst_n = true;
	top->rst_n = true;
	top->eval();
}

void tb_verilator_top::step(const tb_cli_args &args, mem_io_state &memio) {
	(void)args;

	top->clk = false;
	top->eval();

	if (top->i_axi_ar_valid && top->i_axi_ar_ready) {
		bus_request req;
		req.addr = top->i_axi_ar_addr;
		req.size = SIZE_WORD;
		req.write = false;
		req.excl = false;
		req.reservation_id = 0;
		bus_response resp = mem_callback_i(*this, memio, req);
		top->i_axi_r_valid = true;
		top->i_axi_r_data = resp.rdata;
		top->i_axi_r_resp = resp.err ? 2u : 0u;
	} else if (top->i_axi_r_valid && top->i_axi_r_ready) {
		top->i_axi_r_valid = false;
	}

	bool d_r_was_valid = top->d_axi_r_valid;
	bool d_r_was_ready = top->d_axi_r_ready;
	bool d_r_busy = d_r_pending_valid || d_r_was_valid;
	top->d_axi_ar_ready = !d_r_busy;

	if (d_r_was_valid && d_r_was_ready) {
		top->d_axi_r_valid = false;
	}

	if (!d_r_was_valid && d_r_pending_valid) {
		top->d_axi_r_valid = true;
		top->d_axi_r_data = d_r_pending_data;
		top->d_axi_r_resp = d_r_pending_resp;
		d_r_pending_valid = false;
	}

	if (top->d_axi_ar_valid && top->d_axi_ar_ready) {
		bus_request req;
		req.addr = top->d_axi_ar_addr;
		req.size = SIZE_WORD;
		req.write = false;
		req.excl = false;
		req.reservation_id = 1;
		bus_response resp = mem_callback_d(*this, memio, req);
		d_r_pending_valid = true;
		d_r_pending_data = resp.rdata;
		d_r_pending_resp = resp.err ? 2u : 0u;
	}

	if (top->d_axi_aw_valid && top->d_axi_aw_ready) {
		aw_pending = true;
		aw_addr = top->d_axi_aw_addr;
		top->d_axi_w_ready = true;
	}

	if (top->d_axi_w_valid && top->d_axi_w_ready) {
		w_pending = true;
		w_data = top->d_axi_w_data;
		w_strb = top->d_axi_w_strb;
	}

	if (aw_pending && w_pending) {
		bus_response merged_resp;
		if (w_strb == 0x1u || w_strb == 0x2u || w_strb == 0x4u || w_strb == 0x8u) {
			int byte = (w_strb == 0x1u) ? 0 : (w_strb == 0x2u) ? 1 : (w_strb == 0x4u) ? 2 : 3;
			bus_request req;
			req.addr = aw_addr + (uint32_t)byte;
			req.size = SIZE_BYTE;
			req.write = true;
			req.excl = false;
			req.reservation_id = 1;
			req.wdata = (w_data >> (8 * byte)) & 0xffu;
			bus_response resp = mem_callback_d(*this, memio, req);
			merged_resp.err = resp.err;
		} else if (w_strb == 0x3u || w_strb == 0xCu) {
			int half = (w_strb == 0x3u) ? 0 : 2;
			bus_request req;
			req.addr = aw_addr + (uint32_t)half;
			req.size = SIZE_HWORD;
			req.write = true;
			req.excl = false;
			req.reservation_id = 1;
			req.wdata = (w_data >> (8 * half)) & 0xffffu;
			bus_response resp = mem_callback_d(*this, memio, req);
			merged_resp.err = resp.err;
		} else if (w_strb == 0xFu) {
			bus_request req;
			req.addr = aw_addr;
			req.size = SIZE_WORD;
			req.write = true;
			req.excl = false;
			req.reservation_id = 1;
			req.wdata = w_data;
			bus_response resp = mem_callback_d(*this, memio, req);
			merged_resp.err = resp.err;
		} else {
			merged_resp.err = true;
		}

		top->d_axi_b_valid = true;
		top->d_axi_b_resp = merged_resp.err ? 2u : 0u;
		top->d_axi_w_ready = false;
		aw_pending = false;
		w_pending = false;
	} else if (top->d_axi_b_valid && top->d_axi_b_ready) {
		top->d_axi_b_valid = false;
	}

	top->clk = true;
	top->eval();
	++cycle;
}

int main(int argc, char **argv) {
	tb_cli_args args;
	tb_parse_args(argc, argv, args);

	tb_jtag_state jtag(args);
	mem_io_state memio(args);
	if (args.load_bin && tb_verilator_top::TB_BOOT_PC >= MEM_BASE && tb_verilator_top::TB_BOOT_PC < MEM_BASE + MEM_SIZE) {
		uint32_t load_offset = tb_verilator_top::TB_BOOT_PC - MEM_BASE;
		memmove(memio.mem + load_offset, memio.mem, MEM_SIZE - load_offset);
		memset(memio.mem, 0, load_offset);
	}
	tb_verilator_top tb(args, argc, argv);

	bool timed_out = false;
	for (int64_t sim_cycle = 0; sim_cycle < args.max_cycles || args.max_cycles == 0; ++sim_cycle) {
		bool jtag_exit_cmd = jtag.step(tb);
		memio.step(tb);
		tb.step(args, memio);

		if (memio.exit_req) {
			fprintf(tb.logfile, "CPU requested halt. Exit code %d\n", memio.exit_code);
			fprintf(tb.logfile, "Ran for " I64_FMT " cycles\n", sim_cycle + 1);
			break;
		}
		if (sim_cycle + 1 == args.max_cycles) {
			fprintf(tb.logfile, "Max cycles reached\n");
			timed_out = true;
		}
		if (jtag_exit_cmd)
			break;
	}

	jtag.close();

	for (auto r : args.dump_ranges) {
		fprintf(tb.logfile, "Dumping memory from %08x to %08x:\n", r.first, r.second);
		for (int i = 0; i < r.second - r.first; ++i)
			fprintf(tb.logfile, "%02x%c", memio.mem[r.first + i - MEM_BASE], i % 16 == 15 ? '\n' : ' ');
		fprintf(tb.logfile, "\n");
	}

	if (args.sig_path != "") {
		FILE *sigfile = fopen(args.sig_path.c_str(), "wb");
		for (auto r : args.dump_ranges) {
			for (uint32_t i = 0; i < r.second - r.first; i += 4) {
				fprintf(
					sigfile,
					"%02x%02x%02x%02x\n",
					memio.mem[r.first + i + 3 - MEM_BASE],
					memio.mem[r.first + i + 2 - MEM_BASE],
					memio.mem[r.first + i + 1 - MEM_BASE],
					memio.mem[r.first + i + 0 - MEM_BASE]
				);
			}
		}
		fclose(sigfile);
	}

	if (args.propagate_return_code && timed_out) {
		return -1;
	} else if (args.propagate_return_code && memio.exit_req) {
		return memio.exit_code;
	} else {
		return 0;
	}
}

double sc_time_stamp() {
	return 0.0;
}
