# Top-level Makefile for rv1
#
# Targets:
#   rtl            - generate Verilog from SpinalHDL (hw/gen/)
#   sim            - build cxxrtl testbench + run all simulation tests
#   sim-tests      - run simulation tests (testbench already built)
#   sim-tb         - build cxxrtl testbench only
#   fpga-sim       - run fpga GPIO blink test in SpinalHDL simulator (RVSim)
#   synth          - synthesise + place & route for Arty S7 (bitfile)
#   prog           - program Arty S7 via JTAG
#   formal         - run riscv-formal checks
#   clean          - clean generated/build artefacts
#   help           - print this message

.PHONY: all rtl sim sim-tests sim-tb fpga-sim synth prog formal clean help

# ── defaults ──────────────────────────────────────────────────────────────────
BOARD      ?= s7
SIM_TESTS  ?= base mul_hw irq_csr compressed_c hellow debug bus_error

# ── RTL generation ────────────────────────────────────────────────────────────
rtl:
	sbt "runMain rv.RVVerilog"

rtl_fpga:
	sbt "runMain rv.RVTopXilinxVerilog"

# ── Simulation ────────────────────────────────────────────────────────────────
sim: rtl sim-tb sim-tests

sim-tb:
	$(MAKE) -C test/sim/tb_cxxrtl

sim-tests:
	$(MAKE) -C test/sim run

# Individual cxxrtl simulation tests (pass TEST=<name> to run just one)
ifdef TEST
sim-test:
	$(MAKE) -C test/sim/$(TEST) run
else
sim-test:
	@echo "Usage: make sim-test TEST=<testname>   (e.g. TEST=hellow)"
endif

# ── SpinalHDL sim (RVSim) — full SoC with APB peripherals ────────────────────
# Usage:  make fpga-sim          (runs "fpga" test)
#         make fpga-sim SIM_TEST=base
SIM_TEST ?= fpga
fpga-sim:
	sbt "runMain rv.RVSim $(SIM_TEST)"

# ── FPGA synthesis / implementation ──────────────────────────────────────────
synth: rtl_fpga
	$(MAKE) -C soc/synth_vivado BOARD=$(BOARD) bit

prog:
	$(MAKE) -C soc/synth_vivado BOARD=$(BOARD) prog

# ── Formal verification ───────────────────────────────────────────────────────
formal: rtl
	$(MAKE) -C test/formal/riscv-formal/riscv-formal/cores/RV checks

# ── Clean ─────────────────────────────────────────────────────────────────────
clean:
	$(MAKE) -C test/sim          clean
	$(MAKE) -C test/sim/tb_cxxrtl clean
	$(MAKE) -C soc/synth_vivado  clean
	$(MAKE) -C test/formal/riscv-formal/riscv-formal/cores/RV clean

# ── Help ──────────────────────────────────────────────────────────────────────
help:
	@echo ""
	@echo "  make rtl               Generate Verilog from SpinalHDL"
	@echo "  make sim               rtl + testbench + all cxxrtl sim tests"
	@echo "  make sim-tb            Build cxxrtl testbench only"
	@echo "  make sim-tests         Run all cxxrtl sim tests (TB already built)"
	@echo "  make sim-test TEST=X   Run a single cxxrtl test (e.g. hellow)"
	@echo "  make fpga-sim          Run full-SoC SpinalHDL sim (SIM_TEST=fpga)"
	@echo "  make fpga-sim SIM_TEST=base"
	@echo "  make synth [BOARD=s7]  Vivado synthesis + impl (produces bitfile)"
	@echo "  make prog  [BOARD=s7]  Program FPGA via JTAG"
	@echo "  make formal            riscv-formal instruction checks"
	@echo "  make clean             Remove all build artefacts"
	@echo ""
