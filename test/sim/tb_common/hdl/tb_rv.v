// Testbench wrapper for RV core with AXI memory interfaces

`default_nettype none

module tb #(
	parameter W_DATA = 32,
	parameter W_ADDR = 32
) (
	// Global signals
	input wire                clk,
	input wire                rst_n,

	// JTAG port (kept for compatibility, not connected)
	input  wire               tck,
	input  wire               trst_n,
	input  wire               tms,
	input  wire               tdi,
	output wire               tdo,

	// Instruction AXI-Lite interface
	output wire               i_axi_ar_valid,
	input  wire               i_axi_ar_ready,
	output wire [W_ADDR-1:0]  i_axi_ar_addr,
	output wire [2:0]         i_axi_ar_prot,
	input  wire               i_axi_r_valid,
	output wire               i_axi_r_ready,
	input  wire [W_DATA-1:0]  i_axi_r_data,
	input  wire [1:0]         i_axi_r_resp,

	// Data AXI-Lite interface  
	output wire               d_axi_aw_valid,
	input  wire               d_axi_aw_ready,
	output wire [W_ADDR-1:0]  d_axi_aw_addr,
	output wire [2:0]         d_axi_aw_prot,
	output wire               d_axi_w_valid,
	input  wire               d_axi_w_ready,
	output wire [W_DATA-1:0]  d_axi_w_data,
	output wire [3:0]         d_axi_w_strb,
	input  wire               d_axi_b_valid,
	output wire               d_axi_b_ready,
	input  wire [1:0]         d_axi_b_resp,
	output wire               d_axi_ar_valid,
	input  wire               d_axi_ar_ready,
	output wire [W_ADDR-1:0]  d_axi_ar_addr,
	output wire [2:0]         d_axi_ar_prot,
	input  wire               d_axi_r_valid,
	output wire               d_axi_r_ready,
	input  wire [W_DATA-1:0]  d_axi_r_data,
	input  wire [1:0]         d_axi_r_resp,

	// Interrupt
	input wire                irq,
	input wire [1:0]          soft_irq,
	input wire [1:0]          timer_irq
);

assign tdo = 1'b0; // JTAG not connected

RV cpu (
	.io_IO                      (/* open */),
	
	// RVFI (formal verification interface - leave open)
	.rvfi_valid                 (/* open */),
	.rvfi_order                 (/* open */),
	.rvfi_insn                  (/* open */),
	.rvfi_trap                  (/* open */),
	.rvfi_halt                  (/* open */),
	.rvfi_intr                  (/* open */),
	.rvfi_rs1_addr              (/* open */),
	.rvfi_rs2_addr              (/* open */),
	.rvfi_rs1_rdata             (/* open */),
	.rvfi_rs2_rdata             (/* open */),
	.rvfi_rd_addr               (/* open */),
	.rvfi_rd_wdata              (/* open */),
	.rvfi_pc_rdata              (/* open */),
	.rvfi_pc_wdata              (/* open */),
	.rvfi_mem_addr              (/* open */),
	.rvfi_mem_rmask             (/* open */),
	.rvfi_mem_wmask             (/* open */),
	.rvfi_mem_rdata             (/* open */),
	.rvfi_mem_wdata             (/* open */),
	.rvfi_csr_mstatus_wmask     (/* open */),
	.rvfi_csr_mstatus_wdata     (/* open */),
	.rvfi_csr_mepc_wmask        (/* open */),
	.rvfi_csr_mepc_wdata        (/* open */),
	.rvfi_csr_mcause_wmask      (/* open */),
	.rvfi_csr_mcause_wdata      (/* open */),
	.rvfi_ixl                   (/* open */),
	.rvfi_mode                  (/* open */),
	
	// Instruction AXI
	.instr_axi_ar_valid         (i_axi_ar_valid),
	.instr_axi_ar_ready         (i_axi_ar_ready),
	.instr_axi_ar_payload_addr  (i_axi_ar_addr),
	.instr_axi_ar_payload_prot  (i_axi_ar_prot),
	.instr_axi_r_valid          (i_axi_r_valid),
	.instr_axi_r_ready          (i_axi_r_ready),
	.instr_axi_r_payload_data   (i_axi_r_data),
	.instr_axi_r_payload_resp   (i_axi_r_resp),
	
	// Data AXI
	.data_axi_aw_valid          (d_axi_aw_valid),
	.data_axi_aw_ready          (d_axi_aw_ready),
	.data_axi_aw_payload_addr   (d_axi_aw_addr),
	.data_axi_aw_payload_prot   (d_axi_aw_prot),
	.data_axi_w_valid           (d_axi_w_valid),
	.data_axi_w_ready           (d_axi_w_ready),
	.data_axi_w_payload_data    (d_axi_w_data),
	.data_axi_w_payload_strb    (d_axi_w_strb),
	.data_axi_b_valid           (d_axi_b_valid),
	.data_axi_b_ready           (d_axi_b_ready),
	.data_axi_b_payload_resp    (d_axi_b_resp),
	.data_axi_ar_valid          (d_axi_ar_valid),
	.data_axi_ar_ready          (d_axi_ar_ready),
	.data_axi_ar_payload_addr   (d_axi_ar_addr),
	.data_axi_ar_payload_prot   (d_axi_ar_prot),
	.data_axi_r_valid           (d_axi_r_valid),
	.data_axi_r_ready           (d_axi_r_ready),
	.data_axi_r_payload_data    (d_axi_r_data),
	.data_axi_r_payload_resp    (d_axi_r_resp),
	
	.irq                        (irq),
	.clk                        (clk),
	.reset                      (~rst_n)
);

endmodule

`default_nettype wire
