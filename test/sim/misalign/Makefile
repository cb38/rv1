SRCS := ../common/init_rv.S misalign.S
APP  := misalign_rv

CCFLAGS  = -march=rv32i_zicsr -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles
LDSCRIPT = ../common/memmap_rv.ld
TBEXEC   = ../tb_cxxrtl/tb_rv
MAX_CYCLES ?= 100000
RV_ROOT  ?= ../../..
RV_GEN_CMD ?= sbt "runMain rv.RVVerilog"

.DEFAULT_GOAL := run

rv:
	cd $(RV_ROOT) && $(RV_GEN_CMD)

include ../common/src_only_app.mk
