# Top-level Makefile for rv1
#
# Targets:
#   rtl            - generate Verilog from SpinalHDL (hw/gen/)
#   sim            - build cxxrtl testbench + run all simulation tests
#   sim-tests      - run simulation tests (testbench already built)
#   sim-tb         - build cxxrtl testbench only
#   soc-sim        - run a soc/sim test in SpinalHDL simulator (RVSim)
#   synth          - synthesise + place & route for Arty S7 (bitfile)
#   formal         - run riscv-formal checks
#   clean          - clean generated/build artefacts
#   help           - print this message

.PHONY: all rtl sim sim-tests sim-tb soc-sim soc-fw synth formal clean help load_fpga connect_fpga

# ── defaults ──────────────────────────────────────────────────────────────────
BOARD      ?= s7
SIM_TESTS  ?= mul_hw irq_csr compressed_c hellow debug bus_error atomics ecall

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
# Usage:  make soc-sim              (runs "blink" test by default)
#         make soc-sim SOC_TEST=base
SOC_TEST ?= blink
soc-sim:
	sbt "runMain rv.RVSim $(SOC_TEST)"

# ── SoC firmware build ────────────────────────────────────────────────────────
# Compile bare-metal firmware for any test under soc/sim/.
# Pass DEBUG=1 for GDB-friendly build (-g -Og).
# Usage:  make soc-fw              (builds blink by default)
#         make soc-fw SOC_TEST=base
soc-fw:
	$(MAKE) -C soc/sim/$(SOC_TEST) clean
	$(MAKE) -C soc/sim/$(SOC_TEST) bin $(if $(DEBUG),DEBUG=$(DEBUG),)

# ── FPGA synthesis / implementation ──────────────────────────────────────────
synth: rtl_fpga
	$(MAKE) -C soc/synth_vivado BOARD=$(BOARD) bit


# ── Formal verification ───────────────────────────────────────────────────────
formal: rtl
	$(MAKE) -C -j8 test/formal/riscv-formal/riscv-formal/cores/RV checks

# ── Clean ─────────────────────────────────────────────────────────────────────
clean:
	$(MAKE) -C test/sim          clean
	$(MAKE) -C soc/sim/blink     clean
	$(MAKE) -C soc/sim/base      clean
	$(MAKE) -C soc/sim/atomics   clean
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
	@echo "  make soc-sim [SOC_TEST=X]  Run full-SoC SpinalHDL sim (default: blink)"
	@echo "  make soc-fw  [SOC_TEST=X]  Compile soc/sim/X firmware (default: blink)"
	@echo "  make soc-fw  DEBUG=1       … with debug symbols (-g -Og)"
	@echo "  make synth [BOARD=s7]  Vivado synthesis + impl (produces bitfile)"
	@echo "  make formal            riscv-formal instruction checks"
	@echo "  make clean             Remove all build artefacts"
	@echo "  make connect_fpga      Connect to FPGA via OpenOCD (JTAG)"
	@echo "  make gdb_fpga          Connect GDB to FPGA via OpenOCD"
	@echo ""
.PHONY: load_fpga

load_fpga:
	openocd -f soc/prog/7series.txt

connect_fpga:
	openocd -f soc/artys7-openocd.cfg

gdb_fpga:
	riscv64-unknown-elf-gdb -x commands.gdb	

