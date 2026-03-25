SRCS := main.S
APP  := main

CCFLAGS = -march=rv32i -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles
LDSCRIPT = ../common/memmap.ld
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





build: 
	$(MAKE)  bin




include ../common/src_only_app.mk
