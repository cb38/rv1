#pragma once

#include <cstdint>
#include <string>
#include <vector>

struct tb_cli_args {
	bool load_bin;
	std::string bin_path;
	bool dump_waves;
	std::string waves_path;
	std::vector<std::pair<uint32_t, uint32_t>> dump_ranges;
	int64_t max_cycles;
	bool propagate_return_code;
	uint16_t port;
	bool dump_jtag;
	std::string jtag_dump_path;
	bool replay_jtag;
	std::string jtag_replay_path;
	std::string log_path;
	std::string sig_path;
	bool noshift;
	uint32_t dtb_addr;
	bool has_dtb_addr;
	uint32_t boot_pc;
	bool has_boot_pc;
#ifdef CXXRTL_DEBUG_AGENT
	bool run_agent;
#endif
	tb_cli_args() {
		load_bin = false;
		dump_waves = false;
		max_cycles = 0;
		propagate_return_code = false;
		port = 0;
		dump_jtag = false;
		replay_jtag = false;
		noshift = false;
		dtb_addr = 0;
		has_dtb_addr = false;
		boot_pc = 0;
		has_boot_pc = false;
#ifdef CXXRTL_DEBUG_AGENT
		run_agent = false;
#endif
	}
};

void tb_parse_args(int argc, char **argv, tb_cli_args &args);
