
`default_nettype none

module fpga (
	input wire        clk_osc,

	output wire [3:0] led,
	input wire [3:0]  button,
	output wire       uart_tx,
	input  wire       uart_rx
);

wire clk_sys;
wire locked;
wire clkfbout;
wire clkfbout_buf;
BUFG bufg_clkfbout (
	.I (clkfbout),
	.O (clkfbout_buf)
);

// Configured for 12 -> 80 MHz
MMCME2_ADV #(
	.BANDWIDTH            ("OPTIMIZED"),
	.CLKOUT4_CASCADE      ("FALSE"),
	.COMPENSATION         ("ZHOLD"),
	.STARTUP_WAIT         ("FALSE"),
	.DIVCLK_DIVIDE        (1),
	.CLKFBOUT_MULT_F      (60.000),
	.CLKFBOUT_PHASE       (0.000),
	.CLKFBOUT_USE_FINE_PS ("FALSE"),
	.CLKOUT0_DIVIDE_F     (9.000),
	.CLKOUT0_PHASE        (0.000),
	.CLKOUT0_DUTY_CYCLE   (0.500),
	.CLKOUT0_USE_FINE_PS  ("FALSE"),
	.CLKIN1_PERIOD        (83.333)
) mmcm_adv_inst (
	.CLKFBOUT            (clkfbout),
	.CLKFBOUTB           (/* unused */),
	.CLKOUT0             (clk_sys),
	.CLKOUT0B            (/* unused */),
	.CLKOUT1             (/* unused */),
	.CLKOUT1B            (/* unused */),
	.CLKOUT2             (/* unused */),
	.CLKOUT2B            (/* unused */),
	.CLKOUT3             (/* unused */),
	.CLKOUT3B            (/* unused */),
	.CLKOUT4             (/* unused */),
	.CLKOUT5             (/* unused */),
	.CLKOUT6             (/* unused */),
	// Input clock control
	.CLKFBIN             (clkfbout_buf),
	.CLKIN1              (clk_osc),
	.CLKIN2              (1'b0),
	// Tied to always select the primary input clock
	.CLKINSEL            (1'b1),
	// Ports for dynamic reconfiguration
	.DADDR               (7'h0),
	.DCLK                (1'b0),
	.DEN                 (1'b0),
	.DI                  (16'h0),
	.DO                  (/* unused */),
	.DRDY                (/* unused */),
	.DWE                 (1'b0),
	// Ports for dynamic phase shift
	.PSCLK               (1'b0),
	.PSEN                (1'b0),
	.PSINCDEC            (1'b0),
	.PSDONE              (/* unused */),
	// Other control and status signals
	.LOCKED              (locked),
	.CLKINSTOPPED        (/* unused */),
	.CLKFBSTOPPED        (/* unused */),
	.PWRDWN              (1'b0),
	.RST                 (1'b0) // TODO???
);

blinky #(
	.CLK_HZ (12_000_000),
	.BLINK_HZ (1)
) blinky_clk_osc (
	.clk (clk_osc),
	.blink (led[0])
);

blinky #(
	.CLK_HZ (80_000_000),
	.BLINK_HZ (2)
) blinky_clk_sys (
	.clk (clk_sys),
	.blink (led[1])
);

wire rst_n_sys;

fpga_reset #(
	.SHIFT (3)
) rstgen (
	.clk         (clk_sys),
	.force_rst_n (1'b1),
	.rst_n       (rst_n_sys)
);

RVTop  soc_u (
	.clk            (clk_sys),
	.resetn          (rst_n_sys),



	.io_uart_txd        (uart_tx),
	.io_uart_rxd        (uart_rx),
	.io_button_read      (button),
	.io_button_write     (/* unused */),
	.io_button_writeEnable(/* unused */),
	.io_led_read         (/* unused */),
	.io_led_write        (led[3:2]),
	.io_led_writeEnable  (/* unused */)
);

endmodule
