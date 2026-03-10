SRCS       := ../common/init_rv.S src/main.c
APP        := mul_hw
CCFLAGS    := -march=rv32im_zicsr -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles
LDSCRIPT   := ../common/memmap_rv.ld
TBEXEC     := ../tb_cxxrtl/tb_rv
MAX_CYCLES := 200000

TB_MAKEFILE ?= Makefile
TB_DEBUG ?= 0
TB_DEBUG_CYCLES ?= 80
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
