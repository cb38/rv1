SRCS       := ../common/init_rv.S src/main.c
APP        := bus_error
CCFLAGS    := -march=rv32im_zicsr -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles
LDSCRIPT   := ../common/memmap_rv.ld
TBEXEC     := ../tb_cxxrtl/tb_rv
MAX_CYCLES := 50000
RV_ROOT    ?= ../../..
RV_GEN_CMD ?= sbt "runMain rv.RVVerilog"
BUILD_RV   ?= 0
include ../common/src_only_app.mk
