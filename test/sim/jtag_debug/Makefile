SRCS       := ../debug/src/start.S ../debug/src/main.S
APP        := jtag_debug_test
CCFLAGS    := -march=rv32imc_zicsr -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles
LDSCRIPT   := ../mul_hw/memmap_rv.ld
TBEXEC     := ../tb_cxxrtl/tb_jtag_debug
MAX_CYCLES := 500000

TB_MAKEFILE ?= Makefile.jtag_debug
TB_DEBUG ?= 0
TB_DEBUG_CYCLES ?= 8000
RV_ROOT ?= ../../..
RV_GEN_CMD ?= sbt "runMain rv.RVVerilog"

include ../common/src_only_app.mk

.PHONY: rv tb_build check

rv:
	cd $(RV_ROOT) && $(RV_GEN_CMD)

tb_build:
	$(MAKE) -C ../tb_cxxrtl -f $(TB_MAKEFILE) clean
	$(MAKE) -C ../tb_cxxrtl -f $(TB_MAKEFILE) CDEFINES='TB_DEBUG=$(TB_DEBUG) TB_DEBUG_CYCLES=$(TB_DEBUG_CYCLES)'

check: rv tb_build run
