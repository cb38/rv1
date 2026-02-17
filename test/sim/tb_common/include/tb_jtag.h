#include <fstream>
#include <cstdint>

#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "tb.h"
#include "tb_cli.h"

#define TCP_BUF_SIZE 256

struct tb_jtag_state {
	tb_cli_args args;

	int server_fd;
	int sock_fd;
	struct sockaddr_in sock_addr;
	int sock_opt;
	socklen_t sock_addr_len;
	char txbuf[TCP_BUF_SIZE], rxbuf[TCP_BUF_SIZE];
	int rx_ptr;
	int rx_remaining;
	int tx_ptr;

	std::ofstream jtag_dump_fd;
	std::ifstream jtag_replay_fd;

	tb_jtag_state(const tb_cli_args &_args);

	// Returns true if an exit command was received from the JTAG socket
	bool step(tb_top &tb);

	void close();
};

