SRCS       := src/start.S src/main.S
APP        := compressed_c
CCFLAGS    := -march=rv32imc_zicsr -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles
LDSCRIPT   := ../common/memmap_rv.ld
TBEXEC     := ../tb_cxxrtl/tb_rv
MAX_CYCLES := 20000

TB_MAKEFILE ?= Makefile
TB_DEBUG ?= 0
TB_DEBUG_CYCLES ?= 80
RV_ROOT ?= ../../..
RV_GEN_CMD ?= sbt "runMain rv.RVVerilog"

include ../common/src_only_app.mk

.PHONY: rv tb_build check check_disasm

rv:
	cd $(RV_ROOT) && $(RV_GEN_CMD)

tb_build:
	$(MAKE) -C ../tb_cxxrtl -f $(TB_MAKEFILE) clean
	$(MAKE) -C ../tb_cxxrtl -f $(TB_MAKEFILE) CDEFINES='TB_DEBUG=$(TB_DEBUG) TB_DEBUG_CYCLES=$(TB_DEBUG_CYCLES)'

check_disasm: $(TMP_PREFIX)$(APP).bin
	$(CROSS_PREFIX)readelf -A $(TMP_PREFIX)$(APP).elf | grep -Eq "Tag_RISCV_arch: .*_c2p0"

check: rv tb_build check_disasm
