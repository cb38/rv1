SRCS       := ../common/init_rv.S src/main.c
APP        := irq_csr
CCFLAGS    := -march=rv32im_zicsr -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles
LDSCRIPT   := memmap_rv.ld
TBEXEC     := ../tb_cxxrtl/tb_rv
MAX_CYCLES := 300000
TB_MAKEFILE ?= Makefile
TB_DEBUG ?= 0
TB_DEBUG_CYCLES ?= 80
RV_ROOT ?= ../../..
RV_GEN_CMD ?= sbt "runMain rv.RVVerilog"
BUILD_RV ?= 0
include ../common/src_only_app.mk


.PHONY: rv tb_build run chk

rv:
	cd $(RV_ROOT) && $(RV_GEN_CMD)

tb_build:
	$(MAKE) -C ../tb_cxxrtl -f $(TB_MAKEFILE) clean
	$(MAKE) -C ../tb_cxxrtl -f $(TB_MAKEFILE) CDEFINES='TB_DEBUG=$(TB_DEBUG) TB_DEBUG_CYCLES=$(TB_DEBUG_CYCLES)'

check: run
	python3 check_vcd.py $(TMP_PREFIX)$(APP)_run.vcd