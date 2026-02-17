#include "tb_jtag.h"
#include <cstring>

// Stub implementation of JTAG interface - not functional for RVTop
// This is just to satisfy the linker

tb_jtag_state::tb_jtag_state(const tb_cli_args &_args) : args(_args) {
	server_fd = -1;
	sock_fd = -1;
	rx_ptr = 0;
	rx_remaining = 0;
	tx_ptr = 0;
	sock_addr_len = sizeof(sock_addr);
	memset(&sock_addr, 0, sizeof(sock_addr));
	memset(txbuf, 0, TCP_BUF_SIZE);
	memset(rxbuf, 0, TCP_BUF_SIZE);
}

bool tb_jtag_state::step(tb_top &tb) {
	// Stub: no JTAG operations
	return false;
}

void tb_jtag_state::close() {
	// Stub: nothing to close
	if (server_fd >= 0) {
		::close(server_fd);
	}
	if (sock_fd >= 0) {
		::close(sock_fd);
	}
}
