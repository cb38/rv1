# Build the fpga test firmware binary.
# RVSim (sbt "runMain rv.RVSim fpga") will compile and load this automatically.
# Use this Makefile only to produce the .bin for standalone inspection.

SRCS       := src/start.S src/main.c
APP        := fpga
CCFLAGS    := -march=rv32im_zicsr -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles -Wl,--no-warn-rwx-segments
ifeq ($(DEBUG),1)
CCFLAGS    += -g -Og
endif
LDSCRIPT   := memmap.ld
INCDIR     := ../common
MAX_CYCLES := 0    # no run target — test is driven by RVSim
RV_ROOT    ?= ../../..
RV_GEN_CMD ?= sbt "runMain rv.RVVerilog"

.DEFAULT_GOAL := bin
include ../common/src_only_app.mk
