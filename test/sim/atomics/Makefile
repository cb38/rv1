SRCS := ../common/init_rv.S atomics.S
APP  := atomics_rv

CCFLAGS  = -march=rv32ia_zicsr -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles
LDSCRIPT = ../common/memmap_rv.ld
TBEXEC   = ../tb_cxxrtl/tb_rv
MAX_CYCLES ?= 100000
RV_ROOT  ?= ../../..
RV_GEN_CMD ?= sbt "runMain rv.RVVerilog"

.DEFAULT_GOAL := run

rv:
	cd $(RV_ROOT) && $(RV_GEN_CMD)

include ../common/src_only_app.mk
