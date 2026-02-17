SRCS := init_rv.S main.c
APP  := hellow_rv

CCFLAGS = -march=rv32i -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles
LDSCRIPT = memmap_rv.ld
TBEXEC = ../tb_cxxrtl/tb_rv
MAX_CYCLES ?= 100000
TB_DEBUG ?= 0
TB_DEBUG_CYCLES ?= 80
TB_MAKEFILE ?= Makefile
TRACE_ARGS ?=
BUILD_RV ?= 0
RV_ROOT ?= ../../..
RV_GEN_CMD ?= sbt "runMain rv.RVVerilog"

.DEFAULT_GOAL := run

.PHONY: rv tb_build run_dbg 

rv:
	cd $(RV_ROOT) && $(RV_GEN_CMD)

tb_build:
	$(MAKE) -C ../tb_cxxrtl -f $(TB_MAKEFILE) clean
	$(MAKE) -C ../tb_cxxrtl -f $(TB_MAKEFILE) CDEFINES='TB_DEBUG=$(TB_DEBUG) TB_DEBUG_CYCLES=$(TB_DEBUG_CYCLES)'

run_dbg:
	$(MAKE)  tb_build TB_DEBUG=1 TB_DEBUG_CYCLES=$(TB_DEBUG_CYCLES)
	$(MAKE)  run MAX_CYCLES=$(MAX_CYCLES)




include ../common/src_only_app.mk
